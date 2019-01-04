use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

// TODO: Use `&str` or `Cow<'a, str>` instead of a `String`.
#[derive(Clone, Debug, PartialEq)]
enum Token {
    CommandSubstitution(String),
    EndOfCommand,
    String(String),
    VariableSubstitution(String),
}

impl Token {
    fn is_end_of_command(&self) -> bool {
        match self {
            Token::EndOfCommand => true,
            _ => false,
        }
    }
}

///
#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    ExpectedNumberOfArguments(String, u32),
    NoSuchCommand(String),
    NoSuchVariable(String),
}

// TODO: Use a `Cow<'a, str>` instead of a `String`.
/// The result of a command evaluation: either a string or an error.
type Evaluation = Result<String, EvaluationError>;

// TODO: Use a `Cow<'a, str>` instead of a `String`.
/// A function mapped to a command.
type CommandFunction =
    fn(interpreter: &Interpreter, command_name: &str, command_arguments: &[String]) -> Evaluation;

/// The interpreter state.
pub struct Interpreter {
    /// The commands known to the interpreter.
    commands: HashMap<&'static str, CommandFunction>,

    /// The variables known to the interpreter.
    variables: RefCell<HashMap<String, String>>,
}

impl Interpreter {
    /// Adds the given command.
    pub fn add_command(&mut self, command_name: &'static str, command_function: CommandFunction) {
        // TODO: Check that the given command is not yet registered.

        self.commands.insert(command_name, command_function);
    }

    /// Adds the standard commands.
    pub fn add_standard_commands(&mut self) {
        self.add_command(&"puts", command_puts);
        self.add_command(&"set", command_set);
    }

    /// Parses the given text to evaluate the contained commands.
    pub fn evaluate(&self, text: &str) -> Evaluation {
        let mut command_parts: Vec<String> = Vec::new();

        // The result of the last command.
        let mut last_result: Option<String> = None;

        // Create a peekable iterator over the characters of the given text.
        let character_iterator = &mut text.chars().peekable();

        // Iterate over the parsed tokens.
        while let Some(token) = next_token(character_iterator) {
            // The end of the current command is reached.
            if token.is_end_of_command() {
                // Try to evaluate the command tokens.
                last_result = Some(self.evaluate_command(&command_parts)?);

                // Clear the command tokens for the next command.
                command_parts.clear();

                // The whole command is finished.
                if character_iterator.peek().is_none() {
                    break;
                }
            }
            // The end of the current command is not yet reached.
            else {
                // Evaluate the token.
                let token_evaluation = self.evaluate_token(token)?;

                // Collect the token result.
                command_parts.push(token_evaluation);
            }
        }

        if last_result.is_none() {
            return Ok("".to_owned());
        }

        Ok(last_result.unwrap())
    }

    /// Tries to evaluate the command with the given tokens.
    fn evaluate_command(&self, command_parts: &[String]) -> Evaluation {
        // There are no command parts.
        if command_parts.is_empty() {
            return Ok("".to_owned());
        }

        // Get the command name and the command arguments.
        let (ref command_name, command_arguments) = command_parts.split_first().unwrap();

        // Try to get the command function for the given command name.
        let maybe_command_function = self.commands.get::<str>(&command_name);

        // No such command is found.
        if maybe_command_function.is_none() {
            return Err(EvaluationError::NoSuchCommand(command_name.to_string()));
        }

        let command_function = maybe_command_function.unwrap();

        // Call the command function with the command arguments.
        command_function(self, command_name, command_arguments)
    }

    /// Tries to evaluate the given token, which results in a string, if successful.
    fn evaluate_token(&self, token: Token) -> Evaluation {
        match token {
            Token::CommandSubstitution(string) => {
                // Recursively evaluate the command.
                self.evaluate(&string)
            }

            Token::EndOfCommand => unimplemented!(),

            Token::String(string) => {
                // Unwrap the string.
                Ok(string)
            }

            Token::VariableSubstitution(variable) => {
                let maybe_value = self.get_variable(&variable);

                if maybe_value.is_none() {
                    Err(EvaluationError::NoSuchVariable(variable))
                } else {
                    Ok(maybe_value.unwrap())
                }
            }
        }
    }

    fn get_variable(&self, name: &str) -> Option<String> {
        self.variables.borrow().get::<str>(name).cloned()
    }

    /// Instantiates an interpreter.
    pub fn new() -> Interpreter {
        Interpreter {
            commands: HashMap::new(),
            variables: RefCell::new(HashMap::new()),
        }
    }

    fn set_variable(&self, name: String, value: String) {
        self.variables.borrow_mut().insert(name, value);
    }
}

fn character_is_end_of_command(character: char) -> bool {
    character == '\n' || character == ';'
}

pub fn command_puts(
    _interpreter: &Interpreter,
    _command_name: &str,
    command_arguments: &[String],
) -> Evaluation {
    let joined_arguments = command_arguments.join(" ").to_owned();

    println!("{}", joined_arguments);

    Ok(joined_arguments)
}

pub fn command_set(
    interpreter: &Interpreter,
    command_name: &str,
    command_arguments: &[String],
) -> Evaluation {
    if command_arguments.len() != 2 {
        return Err(EvaluationError::ExpectedNumberOfArguments(
            command_name.to_owned(),
            2,
        ));
    }

    let name = &command_arguments[0];
    let value = &command_arguments[1];

    interpreter.set_variable(name.to_string(), value.to_string());

    Ok(command_arguments[1].clone())
}

fn consume_whitespaces(char_iterator: &mut Peekable<Chars>) -> Option<Token> {
    // Try to peek at the next character.
    let next_character = *char_iterator.peek()?;

    // The next character is not a whitespace.
    if !next_character.is_whitespace() {
        return None;
    }

    // TODO: Do not consume when is within quotation.

    // Peek at the next character.
    while let Some(&next_character) = char_iterator.peek() {
        // The next character is a whitespace but not an end of command.
        if next_character.is_whitespace() && !character_is_end_of_command(next_character) {
            char_iterator.next();
        } else {
            break;
        }
    }

    None
}

fn next_command_substitution_token(char_iterator: &mut Peekable<Chars>) -> Option<Token> {
    // Try to peek at the next character.
    let next_character = *char_iterator.peek()?;

    if next_character != '[' {
        return None;
    }

    // TODO: Think of a good capacity.
    let mut string = String::with_capacity(50);

    let mut command_nesting_level = 0;

    // TODO: Do not consume when is within quotation.

    // Peek at the next character.
    while let Some(&next_character) = char_iterator.peek() {
        if next_character == '[' {
            char_iterator.next();
            command_nesting_level += 1;

            if command_nesting_level > 1 {
                string.push(next_character);
            }
        } else if next_character == ']' {
            char_iterator.next();

            if command_nesting_level > 1 {
                string.push(next_character);
            }

            command_nesting_level -= 1;

            if command_nesting_level < 1 {
                break;
            }
        } else {
            char_iterator.next();
            string.push(next_character);
        }
    }

    if command_nesting_level > 0 {
        return None;
    }

    Some(Token::CommandSubstitution(string))
}

fn next_end_of_command_token(char_iterator: &mut Peekable<Chars>) -> Option<Token> {
    // There is a next character.
    if let Some(&next_character) = char_iterator.peek() {
        // The next character is not an end of command.
        if !character_is_end_of_command(next_character) {
            return None;
        }

        // The next character is an end of command.

        // TODO: Do not consume when is within quotation.

        char_iterator.next();
        Some(Token::EndOfCommand)
    }
    // There is no next character.
    else {
        Some(Token::EndOfCommand)
    }
}

fn next_string_token(char_iterator: &mut Peekable<Chars>) -> Option<Token> {
    let string = parse_string(char_iterator)?;

    Some(Token::String(string))
}

fn next_token(char_iterator: &mut Peekable<Chars>) -> Option<Token> {
    // The token parser functions.
    let token_parser_functions = &[
        consume_whitespaces,
        next_end_of_command_token,
        next_command_substitution_token,
        next_variable_substitution_token,
        next_string_token,
    ];

    // Iterate over the token parser functions.
    for token_parser_function in token_parser_functions {
        // Use the current parser function.
        let maybe_token = token_parser_function(char_iterator);

        // A token could be parsed.
        if maybe_token.is_some() {
            return maybe_token;
        }
    }

    None
}

fn next_variable_substitution_token(char_iterator: &mut Peekable<Chars>) -> Option<Token> {
    // Try to peek at the next character.
    let next_character = *char_iterator.peek()?;

    if next_character != '$' {
        return None;
    }

    // Consume the '$'.
    char_iterator.next();

    let string = parse_string(char_iterator)?;

    Some(Token::VariableSubstitution(string))
}

fn parse_string(char_iterator: &mut Peekable<Chars>) -> Option<String> {
    // Try to peek at the next character.
    char_iterator.peek()?;

    // TODO: Think of a good capacity.
    let mut string = String::with_capacity(50);

    // Peek at the next character.
    while let Some(&next_character) = char_iterator.peek() {
        // End of line or whitespace.
        if character_is_end_of_command(next_character) || next_character.is_whitespace() {
            break;
        } else {
            string.push(next_character);
            char_iterator.next();
        }
    }

    Some(string)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_interpreter() {
        let mut interpreter = Interpreter::new();
        interpreter.add_standard_commands();
        interpreter.evaluate(&"puts Hello World");
        interpreter.evaluate(&"[puts Goodbye World]");
        interpreter.evaluate(&"set a 42");
        interpreter.evaluate(&"set b 12");
        interpreter.evaluate(&"puts $a $b");
    }

    #[test]
    fn test_next_token() {
        let texts_and_expected_tokens = &[
            // Single tokens.
            ("", vec![Some(Token::EndOfCommand)]),
            (" ", vec![Some(Token::EndOfCommand)]),
            ("\t", vec![Some(Token::EndOfCommand)]),
            ("\r", vec![Some(Token::EndOfCommand)]),
            ("abc", vec![Some(Token::String("abc".to_owned()))]),
            ("\n", vec![Some(Token::EndOfCommand)]),
            (";", vec![Some(Token::EndOfCommand)]),
            (
                "$blub",
                vec![Some(Token::VariableSubstitution("blub".to_owned()))],
            ),
            (
                "[command1 arg]",
                vec![Some(Token::CommandSubstitution("command1 arg".to_owned()))],
            ),
            (
                "[command2 arg1 arg2]",
                vec![Some(Token::CommandSubstitution(
                    "command2 arg1 arg2".to_owned(),
                ))],
            ),
            (
                "[a [b c]]",
                vec![Some(Token::CommandSubstitution("a [b c]".to_owned()))],
            ),
            // Unbalanced brackets.
            ("[a b", vec![None]),
            ("[a [b c]", vec![None]),
            // Combinations of tokens.
            (
                "foo1 ",
                vec![
                    Some(Token::String("foo1".to_owned())),
                    Some(Token::EndOfCommand),
                ],
            ),
            (
                "foo2 bar   baz",
                vec![
                    Some(Token::String("foo2".to_owned())),
                    Some(Token::String("bar".to_owned())),
                    Some(Token::String("baz".to_owned())),
                    Some(Token::EndOfCommand),
                ],
            ),
            (
                "foo3\nbar;baz",
                vec![
                    Some(Token::String("foo3".to_owned())),
                    Some(Token::EndOfCommand),
                    Some(Token::String("bar".to_owned())),
                    Some(Token::EndOfCommand),
                    Some(Token::String("baz".to_owned())),
                    Some(Token::EndOfCommand),
                ],
            ),
            (
                "puts [set a]",
                vec![
                    Some(Token::String("puts".to_owned())),
                    Some(Token::CommandSubstitution("set a".to_owned())),
                    Some(Token::EndOfCommand),
                ],
            ),
        ];

        // Iterate over the texts and the expected tokens.
        for text_and_expected_tokens in texts_and_expected_tokens {
            let (text, expected_tokens) = text_and_expected_tokens;

            // println!("{:?}", text);

            // Create a peekable character iterator.
            let mut iterator = &mut text.chars().peekable();

            // Iterate over the expected tokens.
            for expected_token in expected_tokens.iter().cloned() {
                let token = next_token(&mut iterator);

                // println!("  {:?}", token);

                assert_eq!(token, expected_token);
            }
        }
    }
}
