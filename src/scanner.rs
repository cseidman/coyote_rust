use std::rc::{Rc};

#[derive(PartialEq)]
#[derive(Clone, Copy, Debug, PartialOrd)]
pub enum TokenType {
    // Single-character tokens.
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
    TOKEN_LEFT_BRACKET, TOKEN_RIGHT_BRACKET,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
    TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
    TOKEN_COLON, TOKEN_DOUBLE_COLON,

    // One or two character tokens.
    TOKEN_BANG, TOKEN_BANG_EQUAL,
    TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER, TOKEN_GREATER_EQUAL,
    TOKEN_LESS, TOKEN_LESS_EQUAL,

    // Literals.
    TOKEN_IDENTIFIER ,

    TOKEN_STRING ,
    TOKEN_BOOL ,
    TOKEN_INTEGER ,
    TOKEN_FLOAT ,

    TOKEN_NIL,

    // Keywords.
    TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
    TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_OR,
    TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
    TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE, TOKEN_BREAK, TOKEN_CONTINUE,

    TOKEN_ERROR, TOKEN_EOF, TOKEN_CR, TOKEN_START
}

use TokenType::* ;

#[derive(PartialEq)]
#[derive(Clone)]
pub struct Token {
    pub tokenType: TokenType,
    pub line: usize,
    pub name: String,
    pub label: &'static str,
}

#[derive(Clone)]
pub struct Scanner {
    code: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
    codeLength: usize
}

impl Scanner {

    pub fn new(source: String) -> Self {

        let code_string:Vec<char> = source.chars().collect();
        let code_len = &code_string.len() ;
        Scanner {
            code: code_string,
            start: 0 ,
            current: 0,
            line: 1,
            codeLength: *code_len
        }
    }

    pub fn LoadSource(&mut self, s: &str) {

        let mut code_string:Vec<char> = s.chars().collect();
        code_string.push('\0') ;
        self.code = code_string.clone() ;
        self.start = 0 ;
        self.current = 0 ;
        self.codeLength = self.code.len() ;
    }

    pub fn scanToken(&mut self) -> Token {

        if self.isAtEnd() {
            return self.makeEOFToken() ;
        }

        self.skipWhitespace();

        self.start = self.current;

        let c = self.advance();

        if self.isAlpha(c) {
            return self.identifier() ;
        }

        if self.isDigit(c) {
            return self.number() ;
        }

        match c {
            '\n'=>  {
                self.line+=1 ;
                self.makeToken(TOKEN_CR)
            },
            '\0'=>  self.makeToken(TOKEN_EOF),
            '(' =>  self.makeToken(TOKEN_LEFT_PAREN),
            ')' =>  self.makeToken(TOKEN_RIGHT_PAREN),
            '{' =>  self.makeToken(TOKEN_LEFT_BRACE),
            '}' =>  self.makeToken(TOKEN_RIGHT_BRACE),
            '[' =>  self.makeToken(TOKEN_LEFT_BRACKET),
            ']' =>  self.makeToken(TOKEN_RIGHT_BRACKET),
            ';' =>  self.makeToken(TOKEN_SEMICOLON),
            ',' =>  self.makeToken(TOKEN_COMMA),
            '.' =>  self.makeToken(TOKEN_DOT),
            '-' =>  self.makeToken(TOKEN_MINUS),
            '+' =>  self.makeToken(TOKEN_PLUS),
            '/' =>  self.makeToken(TOKEN_SLASH),
            '*' =>  self.makeToken(TOKEN_STAR),
            ':' =>  if self.tmatch(':') {
                        self.makeToken(TOKEN_DOUBLE_COLON)
                    } else {
                        self.makeToken(TOKEN_COLON)
                    },

            '!' =>
                 if self.tmatch('=') { self.makeToken(TOKEN_BANG_EQUAL) }
                else { self.makeToken(TOKEN_BANG) },
            '=' =>
                 if self.tmatch('=') { self.makeToken(TOKEN_EQUAL_EQUAL) }
                else { self.makeToken(TOKEN_EQUAL) },
            '<' =>
                 if self.tmatch('=') { self.makeToken(TOKEN_LESS_EQUAL) } else { self.makeToken(TOKEN_LESS) },
            '>' =>
                 if self.tmatch('=') { self.makeToken(TOKEN_GREATER_EQUAL) } else { self.makeToken(TOKEN_GREATER) },
            '"' =>  self.string(),
            _ => panic!("Unexpected character: {}", c),
        }
    }

    fn errorToken(&mut self, message: &'static str) -> Token {
         Token {
            tokenType: TOKEN_ERROR,
            line: self.line,
            name: message.to_string(),
            label: "TOKEN_ERROR"
        }
    }

    fn tmatch(&mut self, expected: char) -> bool {
        if self.isAtEnd() {
            return false ;
        }
        if self.currentChar() != expected {
             return false ;
        }

        self.current += 1;
        true
    }

    fn isDigit(&mut self, c: char) -> bool {
         c.is_ascii_digit()
    }

    fn number(&mut self) -> Token {
        let mut isFloat  = false ;
        loop  {
            let selfPeek= self.peek() ;
            if self.isDigit(selfPeek) {
                self.advance();
            } else {
                break ;
            }
        }

        let peekNext = self.peekNext() ;
        if self.peek() == '.' && self.isDigit(peekNext) {
            isFloat = true ;
            self.advance();
            loop  {
                let selfPeek= self.peek() ;
                if self.isDigit(selfPeek) {
                    self.advance();
                } else {
                    break ;
                }
            }
        }
        if isFloat {
            self.makeToken(TOKEN_FLOAT)
        } else {
            self.makeToken(TOKEN_INTEGER)
        }

    }

    fn string(&mut self) -> Token {
        loop {
            // Keep going until we hit a closing quote
            if self.peek() != '"' && !self.isAtEnd() {
                if self.peek() == '\n' {
                    self.line += 1;
                }
                self.advance();
            } else {
                break;
            }
        }

        // Unterminated string
        if self.isAtEnd() {
            return self.errorToken("Unterminated string.") ;
        }

        // Close out the string
        self.advance();
        self.makeToken(TOKEN_STRING)
    }

    /* ------------------------------------------------------------------
    Brings back either an identifier name such as a variable or a keyword
    ---------------------------------------------------------------------*/
    fn identifier(&mut self) -> Token {
        loop {
            let peek = self.peek() ;

            if peek == '\n' || peek == '\r' {
                break ;
            }

            if self.isAlpha(peek) {
                self.advance();
            } else {
                break ;
            }
        }
        let idType = self.identifierType() ;
        self.makeToken(idType)
    }

    fn currentChar(&mut self) -> char {
         self.code[self.current]
    }

    fn prevChar(&mut self) -> char{
        self.code[self.current-1]
    }

    fn nextChar(&mut self) -> char{
        self.code[self.current+1]
    }

    pub fn peek(&mut self) -> char {
        self.currentChar()
    }

    pub fn peekNext(&mut self) -> char {
        if self.isAtEnd() {
            '\0'
        } else {
            self.nextChar()
        }
    }

    pub fn advance(&mut self) -> char {
        if self.current == self.code.len() {
            return '\0';
        }
        self.current += 1;
        self.code[self.current-1]
    }

    fn makeToken(&self, ttype: TokenType) -> Token {

        let slice: Vec<char> = self.code[self.start..self.current].to_vec();

        Token {
            tokenType: ttype,
            line: self.line,
            name: slice.iter().clone().collect::<String>(),
            label: stringify!(ttype),
        }
    }

    fn makeEOFToken(&self) -> Token {
        Token {
            tokenType: TOKEN_EOF,
            line: 0,
            name: "EOF".to_string(),
            label: "TOKEN_EOF",
        }
    }

    // Convert a slice of character vectors from the token into a string
    fn getTokenValue(&self) -> String {
        let vecSlice = &self.code[self.start..=self.current-1] ;
        vecSlice.iter().collect()
    }

    fn isAtEnd(&mut self) -> bool {
        self.current >= self.code.len()-1
    }

    fn isAlpha(&mut self, c: char) -> bool {
        c.is_alphabetic()
    }

    fn skipWhitespace(&mut self) {
        loop {
            if self.isAtEnd() {
                break ;
            }
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {self.advance();},
                '\n' => {
                    self.line += 1;
                    self.advance();
                },
                '/' => {
                    if self.peekNext() == '/' {
                        // A comment goes until the end of the line.
                        loop {
                            if self.peek() != '\n' && !self.isAtEnd() {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    } else if self.peekNext() == '*' {
                        // This is the beginning of a block comment
                        self.advance(); // Consume the '*'
                        loop {
                            if self.peek() == '*' {
                                self.advance();

                                if self.peek() == '/' {
                                    self.advance();
                                    // By now we have */ to close off the comments
                                     break ;
                                }
                            }
                            // As long as we haven't reached the end of the comment .. keep
                            // eating up chars
                            if !self.isAtEnd() {
                                self.advance();
                            } else {
                                break ;
                            }
                        }
                    } else {
                        break ;
                    }
                }
                _ => return
            }
        }
    }

    fn identifierType(&mut self) -> TokenType {

        let tokenString = self.getTokenValue() ;
        // If this identifier is a keyword, then return the appropriate token
        match tokenString.to_lowercase().as_str() {
            "and"       => TOKEN_AND,
            "class"     => TOKEN_CLASS,
            "else"      => TOKEN_ELSE,
            "false"     => TOKEN_FALSE,
            "for"       => TOKEN_FOR,
            "fun"       => TOKEN_FUN,
            "if"        => TOKEN_IF,
            "nil"       => TOKEN_NIL,
            "or"        => TOKEN_OR,
            "print"     => TOKEN_PRINT,
            "return"    => TOKEN_RETURN,
            "super"     => TOKEN_SUPER,
            "this"      => TOKEN_THIS,
            "true"      => TOKEN_TRUE,
            "let"       => TOKEN_VAR,
            "while"     => TOKEN_WHILE,
            "break"     => TOKEN_BREAK,
            "continue"  => TOKEN_CONTINUE,
             _          => TOKEN_IDENTIFIER ,
        }

    }

}
