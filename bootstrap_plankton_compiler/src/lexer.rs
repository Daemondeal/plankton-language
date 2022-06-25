use crate::{
    compiler::FileId,
    token::{Token, TokenKind},
    PlanktonError, Span,
};

pub fn tokenize(file: &str, id: FileId) -> Result<Vec<Token>, Vec<PlanktonError>> {
    let mut lexer = Lexer::new(file, id);
    lexer.tokenize()
}

pub struct Lexer {
    file: FileId,
    code: String,
    chars: Vec<char>,

    start: usize,
    current: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: &str, file: FileId) -> Self {
        let code = source.to_owned();
        let chars: Vec<char> = code.chars().collect();

        Lexer {
            code,
            chars,
            file,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Vec<PlanktonError>> {
        let mut tokens = vec![];
        let mut errors = vec![];

        while !self.is_at_end() {
            self.start = self.current;

            match self.scan_token() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => {}
                Err(error) => errors.push(error),
            };
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        // FIXME: This is a hack, the parser shouldn't need a LineEnd even in the end
        tokens.push(self.make_token(TokenKind::LineEnd).unwrap());

        tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(self.file, self.current, self.current),
        });

        Ok(tokens)
    }

    fn scan_token(&mut self) -> Result<Option<Token>, PlanktonError> {
        let c = self.advance();

        match c {
            '(' => Ok(self.make_token(TokenKind::LeftParen)),
            ')' => Ok(self.make_token(TokenKind::RightParen)),
            ',' => Ok(self.make_token(TokenKind::Comma)),
            '+' => Ok(self.make_token(TokenKind::Plus)),
            '*' => Ok(self.make_token(TokenKind::Star)),
            '/' => Ok(self.make_token(TokenKind::Slash)),
            ':' => Ok(self.make_token(TokenKind::Colon)),
            '{' => Ok(self.make_token(TokenKind::LeftBrace)),
            '}' => Ok(self.make_token(TokenKind::RightBrace)),
            '[' => Ok(self.make_token(TokenKind::LeftBracket)),
            ']' => Ok(self.make_token(TokenKind::RightBracket)),
            '&' => Ok(self.make_token(TokenKind::Ampersand)),

            '-' => {
                if self.match_char('>') {
                    Ok(self.make_token(TokenKind::Arrow))
                } else {
                    Ok(self.make_token(TokenKind::Minus))
                }
            }

            '!' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::BangEqual))
                } else {
                    Err(PlanktonError::LexerError {
                        message: "Unexpected Character".to_owned(),
                        span: self.get_span(),
                    })
                }
            }
            '=' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::EqualEqual))
                } else {
                    Ok(self.make_token(TokenKind::Equal))
                }
            }
            '<' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::LessEqual))
                } else {
                    Ok(self.make_token(TokenKind::Less))
                }
            }
            '>' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::GreaterEqual))
                } else {
                    Ok(self.make_token(TokenKind::Greater))
                }
            }
            '.' => {
                if self.match_char('.') {
                    Ok(self.make_token(TokenKind::DotDot))
                } else {
                    Ok(self.make_token(TokenKind::Dot))
                }
            }

            // Comment
            '#' => {
                while self.peek() != '\n' && !self.is_at_end() {
                    self.advance();
                }
                Ok(None)
            }

            ' ' | '\r' | '\t' => Ok(None),

            '\n' => {
                self.line += 1;
                Ok(self.make_token(TokenKind::LineEnd))
            }

            '"' => self.make_string(),

            '0'..='9' => Ok(self.make_number()),
            'a'..='z' | 'A'..='Z' | '_' => Ok(self.make_identifier()),

            _ => Err(PlanktonError::LexerError {
                message: "Unexpected Character".to_owned(),
                span: self.get_span(),
            }),
        }
    }

    fn make_token(&self, token_type: TokenKind) -> Option<Token> {
        Some(Token {
            kind: token_type,
            span: self.get_span(),
        })
    }

    // FIXME: This is a terrible implementation. Find a better way to do this that supports more types of integers
    fn make_number(&mut self) -> Option<Token> {
        let mut is_integer: bool = true;

        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            is_integer = false;
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        // FIXME: Unwrapping here should be fine, but I should probably still properly handle the error.
        let num_text = &self.code[self.start..self.current];
        if is_integer {
            self.make_token(TokenKind::Integer32(num_text.parse::<i32>().unwrap()))
        } else {
            self.make_token(TokenKind::Float32(num_text.parse::<f32>().unwrap()))
        }
    }

    fn make_string(&mut self) -> Result<Option<Token>, PlanktonError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(PlanktonError::LexerError {
                message: "Unterminated string".to_string(),
                span: self.get_span(),
            });
        }

        // Consumes the closing '"'
        self.advance();

        Ok(self.make_token(TokenKind::String(
            self.code[self.start + 1..self.current - 1].to_string(),
        )))
    }

    fn make_identifier(&mut self) -> Option<Token> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = &self.code[self.start..self.current];

        let token_type = match text {
            "not" => TokenKind::Not,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "proc" => TokenKind::Proc,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "let" => TokenKind::Let,
            _ => TokenKind::Identifier(text.to_string()),
        };

        self.make_token(token_type)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn get_span(&self) -> Span {
        Span::new(self.file, self.start, self.current)
    }

    fn advance(&mut self) -> char {
        self.current += 1;

        self.chars[self.current - 1]
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.chars[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 > self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.chars[self.current] != expected {
            return false;
        }

        self.current += 1;
        true
    }
}
