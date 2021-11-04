
pub enum TokenType {
    // Single-character tokens.
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
    TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
    // One or two character tokens.
    TOKEN_BANG, TOKEN_BANG_EQUAL,
    TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER, TOKEN_GREATER_EQUAL,
    TOKEN_LESS, TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
    TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
    TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
    TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,

    TOKEN_ERROR, TOKEN_EOF
}

#[derive(PartialEq)]
#[derive(Clone)]
pub struct Token {
    pub tokenType: TokenType,
    pub line: usize,
    pub name: String,
    pub label: String,
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

    pub fn new() -> Self {
        let s = "" ;
        let code_string:Vec<char> = s.chars().collect();
        return Scanner{
            code: code_string.clone(),
            start: 0 ,
            current: 0,
            line: 0,
            codeLength: code_string.len()
        }
    }

    pub fn LoadSource(&mut self, s: &String) {

        let mut code_string:Vec<char> = s.chars().collect();
        code_string.push('\0') ;
        self.code = code_string.clone() ;
        self.start = 0 ;
        self.current = 0 ;
        self.codeLength = self.code.len() ;
    }

    pub fn ScanToken(&mut self) -> Token {

        if self.isAtEnd() {
            return self.makeEOFToken();
        }

        self.skipWhitespace();

        self.start = self.current;

        let c = self.advance();

        if self.isAlpha(c) {
            return self.identifier();
        }

        if self.isDigit(c) {
            return self.number() ;
        }

        match c {
            '\n'=> return self.makeToken(T_CR),
            '\0'=> return self.makeToken(T_EOF),
            '(' => return self.makeToken(T_LEFT_PAREN),
            ')' => return self.makeToken(T_RIGHT_PAREN),
            '{' => return self.makeToken(T_LEFT_BRACE),
            '}' => return self.makeToken(T_RIGHT_BRACE),
            '[' => return self.makeToken(T_LEFT_BRACKET),
            ']' => return self.makeToken(T_RIGHT_BRACKET),
            ';' => return self.makeToken(T_SEMICOLON),
            ',' => return self.makeToken(T_COMMA),
            '.' => return self.makeToken(T_DOT),
            '-' => return self.makeToken(T_MINUS),
            '+' => return self.makeToken(T_PLUS),
            '/' => return self.makeToken(T_SLASH),
            '*' => return self.makeToken(T_STAR),
            ':' => return if self.tmatch(':') {
                        self.makeToken(T_DOUBLE_COLON)
                    } else {
                        self.makeToken(T_COLON)
                    },

            '!' =>
                return if self.tmatch('=') { self.makeToken(T_BANG_EQUAL) }
                else { self.makeToken(T_BANG) },
            '=' =>
                return if self.tmatch('=') { self.makeToken(T_EQUAL_EQUAL) }
                else { self.makeToken(T_EQUAL) },
            '<' =>
                return if self.tmatch('=') { self.makeToken(T_LESS_EQUAL) } else { self.makeToken(T_LESS) },
            '>' =>
                return if self.tmatch('=') { self.makeToken(T_GREATER_EQUAL) } else { self.makeToken(T_GREATER) },
            '"' => return self.string(),
            _ => panic!("Unexpected character: {}", c),
        }
    }

    //pub fn StartToken(&mut self) -> Token {
    //    return self.makeToken(T_START) ;
    //}

    fn errorToken(&mut self, message: &str) -> Token {
        return Token {
            tokenType: T_ERROR,
            line: self.line,
            name: String::from(message).chars().collect(),
            label: "T_ERROR".to_string()
        }
    }

    fn tmatch(&mut self, expected: char) -> bool {
        if self.isAtEnd() { return false };
        if self.currentChar() != expected {
            return false
        };

        self.current += 1;
        return true;
    }

    fn isDigit(&mut self, c: char) -> bool {
        return c.is_ascii_digit();
    }

    fn number(&mut self) -> Token {
        let mut isFloat  = false ;
        loop {

            let mut selfPeek = self.peek() ;
            let mut isDigit  = self.isDigit(selfPeek) ;

            if isDigit {
                self.advance();

                // If it's a float ..
                selfPeek = self.peek() ;
                isDigit = self.isDigit(selfPeek) ;
                if selfPeek == '.' && isDigit {
                    // Consume the ".".
                    isFloat = true ;
                    self.advance();
                    loop {
                        selfPeek = self.peek() ;
                        isDigit = self.isDigit(selfPeek) ;
                        if isDigit {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
            } else {
                break;
            }
        }
        if isFloat {
            return self.makeToken(T_FLOAT);
        } else {
            return self.makeToken(T_INTEGER);
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
            return self.errorToken("Unterminated string.");
        }

        // Close out the string
        self.advance();
        return self.makeToken(T_STRING);
    }

    /* ------------------------------------------------------------------
    Brings back either an identifier name such as a variable or a keyword
    ---------------------------------------------------------------------*/
    fn identifier(&mut self) -> Token {
        loop {
            let peek = self.peek() ;

            if self.isAlpha(peek) {
                self.advance();
            } else {
                break ;
            }
        }
        let idType = self.identifierType() ;
        return self.makeToken(idType) ;
    }

    fn currentChar(&mut self) -> char {
        return self.code[self.current];
    }

    fn prevChar(&mut self) -> char{
        return self.code[self.current-1];
    }

    fn nextChar(&mut self) -> char{
        return self.code[self.current+1];
    }

    pub fn peek(&mut self) -> char {
        return self.currentChar();
    }

    pub fn peekNext(&mut self) -> char {
       return self.nextChar();
    }

    pub fn advance(&mut self) -> char {
        self.current += 1;
        return self.code[self.current-1];
    }

    fn makeToken(&self, ttype: TokenType) -> Token {

        let slice:Vec<char> = self.code[self.start..self.current].to_vec();

        return Token {
            tokenType: ttype,
            line: 0,
            name: slice.into_iter().collect(),
            label: stringify!(ttype).to_string(),
        }
    }

    fn makeEOFToken(&self) -> Token {
        return Token {
            tokenType: T_EOF,
            line: 0,
            name: String::from("EOF"),
            label: "T_EOF".to_string(),
        }
    }

    // Convert a slice of character vectors from the token into a string
    fn getTokenValue(&self) -> String {
        let vecSlice = &self.code[self.start..=self.current] ;
        let s: String = vecSlice.into_iter().collect();
        return s ;
    }

    fn isAtEnd(&mut self) -> bool {
        return self.current >= self.code.len();
    }

    fn isAlpha(&mut self, c: char) -> bool {
        return c.is_alphabetic() ;
    }

    fn skipWhitespace(&mut self) {
        loop {
            if self.isAtEnd() {
                return ;
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
                        return ;
                    }
                }
                _ => return
            }
        }
    }

    fn identifierType(&mut self) -> TokenType {

        let tokenString = self.getTokenValue() ;
        // If this identifier is a keyword, then return the appropriate token
        return match tokenString.as_str() {
            "and"       => T_AND,
            "class"     => T_CLASS,
            "else"      => T_ELSE,
            "false"     => T_FALSE,
            "for"       => T_FOR,
            "fun"       => T_FUN,
            "if"        => T_IF,
            "nil"       => T_NIL,
            "or"        => T_OR,
            "print"     => T_PRINT,
            "return"    => T_RETURN,
            "super"     => T_SUPER,
            "this"      => T_THIS,
            "true"      => T_TRUE,
            "var"       => T_VAR,
            "while"     => T_WHILE,
             _          => T_IDENTIFIER ,
        }

    }

}
