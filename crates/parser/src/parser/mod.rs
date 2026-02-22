mod expr;
mod binop;
mod helpers;
mod statement;
mod postfix;

use logos::Span;
use std::iter::Peekable;

use crate::ast::*;
use crate::lexer::{SpannedToken, Token};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError at {:?}: {}", self.span, self.message)
    }
}

impl std::error::Error for ParseError {}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<I: Iterator<Item = SpannedToken>> {
    tokens: Peekable<I>,
    current_span: Span,
    /// Stores the source text for slicing identifiers / literals
    source: String,
}

impl<I: Iterator<Item = SpannedToken>> Parser<I> {
    pub fn new(source: &str, iterator: I) -> Self {
        Self {
            tokens: iterator.peekable(),
            current_span: 0..0,
            source: source.to_string(),
        }
    }

    /// Peek at the token, returning a clone (avoids borrow issues).
    fn peek_token(&mut self) -> Token {
        self.tokens
            .peek()
            .map(|(t, _)| t.clone())
            .unwrap_or(Token::Eof)
    }

    /// Peek at the span of the next token.
    fn peek_span(&mut self) -> Span {
        self.tokens
            .peek()
            .map(|(_, s)| s.clone())
            .unwrap_or(self.current_span.clone())
    }

    /// Advances to the next token and returns it, updating the current span.
    fn advance(&mut self) -> Option<Token> {
        if let Some((token, span)) = self.tokens.next() {
            self.current_span = span;
            Some(token)
        } else {
            None
        }
    }

    /// Returns the source text for a span.
    fn text(&self, span: &Span) -> &str {
        &self.source[span.start..span.end]
    }

    /// Returns the source text of the last consumed token.
    fn last_text(&self) -> &str {
        self.text(&self.current_span)
    }

    /// Consumes the next token if it matches `expected`, else returns an error.
    fn expect(&mut self, expected: Token) -> ParseResult<Span> {
        let tok = self.peek_token();
        let span = self.peek_span();
        if tok == expected {
            self.advance();
            Ok(self.current_span.clone())
        } else {
            Err(ParseError {
                message: format!("expected {:?}, found {:?}", expected, tok),
                span,
            })
        }
    }

    /// Consume an identifier and return its text. Error if not a Name.
    fn expect_name(&mut self) -> ParseResult<String> {
        let tok = self.peek_token();
        let span = self.peek_span();
        if tok == Token::Name {
            self.advance();
            Ok(self.last_text().to_string())
        } else {
            Err(ParseError {
                message: format!("expected identifier, found {:?}", tok),
                span,
            })
        }
    }

    /// Skips over any Newline tokens.
    fn skip_newlines(&mut self) {
        while self.peek_token() == Token::Newline {
            self.advance();
        }
    }

    /// Checks if the next token matches, consuming it if so.
    fn eat(&mut self, expected: Token) -> bool {
        if self.peek_token() == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    // --- Top-level ---

    /// Parse a complete module (sequence of statements until EOF).
    pub fn parse_module(&mut self) -> ParseResult<Module> {
        let mut body = Vec::new();
        self.skip_newlines();
        while self.peek_token() != Token::Eof {
            let stmt = self.parse_stmt()?;
            body.push(stmt);
            self.skip_newlines();
        }
        Ok(Module { body })
    }

    // --- Block ---

    /// Parse an indented block: NEWLINE INDENT stmt+ DEDENT
    fn parse_block(&mut self) -> ParseResult<Vec<Spanned<Stmt>>> {
        self.expect(Token::Newline)?;
        self.expect(Token::Indent)?;
        let mut stmts = Vec::new();
        self.skip_newlines();
        while self.peek_token() != Token::Dedent && self.peek_token() != Token::Eof {
            stmts.push(self.parse_stmt()?);
            self.skip_newlines();
        }
        self.expect(Token::Dedent)?;
        if stmts.is_empty() {
            return Err(ParseError {
                message: "expected an indented block".into(),
                span: self.current_span.clone(),
            });
        }
        Ok(stmts)
    }
}