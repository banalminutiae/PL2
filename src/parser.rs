use crate::ast::{Identifier, LetStatement, Program, ReturnStatement, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    next_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let curr_token = lexer.next_token();
        let next_token = lexer.next_token();
        Self {
            lexer,
            curr_token,
            next_token,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        while self.curr_token.token_type != TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token.token_type {
            TokenType::LET => self.parse_let_statement().map(Statement::Let),
            TokenType::RETURN => self.parse_return_statement().map(Statement::Return),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let current_token = self.curr_token.clone();

        if !self.peek_and_consume_on_match(TokenType::IDENTIFIER) {
            return None;
        }

        let statement = LetStatement {
            token: current_token.clone(),
            name: Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            },
            // Expression here later
        };

        if !self.peek_and_consume_on_match(TokenType::ASSIGN) {
            return None;
        }

        // Temporarily skip expression parsing
        while self.curr_token.token_type != TokenType::SEMICOLON {
            self.next_token()
        }

        Some(statement)
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let statement = ReturnStatement {
            token: self.curr_token.clone(),
        };

        // Temporarily skip expression parsing
        while self.curr_token.token_type != TokenType::SEMICOLON {
            self.next_token()
        }

        Some(statement)
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let message = format!(
            "Expected next token to be {:?}, got {:?} instead",
            token_type, self.next_token.token_type
        );
        self.errors.push(message);
    }

    fn peek_and_consume_on_match(&mut self, expected_type: TokenType) -> bool {
        if self.next_token.token_type == expected_type {
            self.next_token();
            true
        } else {
            self.peek_error(expected_type);
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statement() {
        let source = r#"
           let x = 5;
           let y = 10;

           let foobar = 828282;
        "#;

        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 3);
        println!("{:#?}", program.statements);
    }

    #[test]
    fn test_return_statement() {
        let source = r#"
            return 5;
        "#;
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        println!("{:?}", program.statements);
        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parser_error() {
        let source = r#"
            let = 5;
            let 828282;
        "#;

        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse_program();
        assert_eq!(parser.errors.len(), 2);
        println!("{:?}", parser.errors);
    }
}
