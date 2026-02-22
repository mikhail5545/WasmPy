use logos::{Logos,Span};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\f]+")] // skip spaces/tabs/form feed but not newlines
pub enum Token{
    #[regex(r"\r?\n")]
    Newline,

    Indent,
    Dedent,

    // Keywords
    #[token("def")] Def,
    #[token("class")] Class,
    #[token("if")] If,
    #[token("else")] Else,
    #[token("elif")] Elif,
    #[token("for")] For,
    #[token("while")] While,
    #[token("break")] Break,
    #[token("continue")] Continue,
    #[token("return")] Return,
    #[token("import")] Import,
    #[token("from")] From,
    #[token("as")] As,
    #[token("pass")] Pass,
    #[token("raise")] Raise,
    #[token("try")] Try,
    #[token("except")] Except,
    #[token("finally")] Finally,
    #[token("with")] With,
    #[token("lambda")] Lambda,
    #[token("yield")] Yield,
    #[token("global")] Global,
    #[token("nonlocal")] Nonlocal,
    #[token("assert")] Assert,
    #[token("del")] Del,
    #[token("async")] Async,
    #[token("await")] Await,
    #[token("True")] True,
    #[token("False")] False,
    #[token("None")] None_,

    // Multi-char operators
    #[token("**")] Pow,
    #[token("//")] FloorDiv,
    #[token("<<")] ShiftLeft,
    #[token(">>")] ShiftRight,
    #[token("==")] Eq,
    #[token("!=")] NotEq,
    #[token("<=")] Le,
    #[token(">=")] Ge,
    #[token(":=")] Walrus,
    #[token("->")] Arrow,

    // Single-char operators
    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Star,
    #[token("/")] Slash,
    #[token("%")] Percent,
    #[token("@")] At,
    #[token("&")] Amp,
    #[token("|")] Pipe,
    #[token("^")] Caret,
    #[token("~")] Tilde,
    #[token("<")] Lt,
    #[token(">")] Gt,
    #[token("=")] Assign,
    #[token(":")] Colon,
    #[token(".")] Dot,
    #[token(",")] Comma,
    #[token(";")] Semicolon,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LBracket,
    #[token("]")] RBracket,
    #[token("{")] LBrace,
    #[token("}")] RBrace,

    // Literals and identifiers
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Name,

    #[regex(r"[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?")]
    Number,

    // Simple string patterns (covers single- and double-quoted; triple-quoted and raw/bytes require more handling)
    #[regex(r#"'([^'\\\n]|\\.)*'|"([^"\\\n]|\\.)*""#)]
    String,

    // Comments
    #[regex(r"#[^\n]*+")]
    Comment,

    // End of input (Logos provides `.slice()` and `logos::Lexer::extras` — explicit EOF variant optional)
    Eof,

    // Logos error fallback
    Error,
}
pub type SpannedToken = (Token, Span);

/// Wraps the Logos lexer and emits 'Indent' / 'Dedent' tokens based on
/// leading whitespace after each newline, following Python's indentation rules.
pub struct IndentLexer<'src> {
    source: &'src str,
    /// Pre-collected raw tokens from Logos (with spans)
    raw: Vec<SpannedToken>,
    /// Current position inside `raw`.
    pos: usize,
    /// Stack of indentation column widths; always starts with `[0]`
    indent_stack: Vec<usize>,
    /// Pending synthetic tokens to emit before continuing with `raw`
    pending: Vec<SpannedToken>,
    /// Whether we have already emitted the final `EOF`
    finished: bool,
    /// Tracks bracket nesting depth so that indentation is ignored inside
    /// `()`, `[]`, `{}` (implicit line continuation)
    bracket_depth: usize,
}

impl<'src> IndentLexer<'src> {
    pub fn new(source: &'src str) -> Self{
        // Collect every token Logos produces
        let raw = Token::lexer(source)
            .spanned()
            .map(|(token, span)| {
                match token {
                    Ok(token) => (token, span),
                    Err(_) => (Token::Error, span),
                }
            }).collect();


        Self{
            source,
            raw,
            pos: 0,
            indent_stack: vec![0],
            pending: Vec::new(),
            finished: false,
            bracket_depth: 0,
        }
    }

    /// Measure the number of leading whitespace columns starting from `start` byte offset in `source`.
    ///  Tabs count as 8-space stops (e.g. if you're at column 5 and see a tab, you move to column 8; if you're at column 8 and see a tab, you move to column 16).
    fn measure_indent(&self, start: usize) -> usize {
        let mut col = 0;
        for ch in self.source[start..].chars() {
            match ch{
                ' ' => col += 1,
                '\t' => col = (col / 8 + 1) * 8, // tabs to next multiple of 8
                _ => break,
            }
        }
        col
    }

    /// Emit Indent/Dedent tokens by comparing `new_indent` against the current top of `indent_stack`.
    /// `span` is used for any synthetic tokens to point to the line that caused the indent change (for error reporting).
    fn handle_indent_change(&mut self, new_indent: usize, span: Span) {
        let current = *self.indent_stack.last().unwrap();

        if new_indent > current {
            self.indent_stack.push(new_indent);
            self.pending.push((Token::Indent, span));
        } else if new_indent < current {
            // Pop until we find a matching indent level
            while let Some(&top) = self.indent_stack.last() {
                if top <= new_indent {
                    break;
                }
                self.indent_stack.pop();
                self.pending.push((Token::Dedent, span.clone()));
            }
            // if the indent level doesn't match any outer block, it's an error (unindent does not match any outer level)
            if *self.indent_stack.last().unwrap() != new_indent {
                self.pending.push((Token::Error, span));
            }
        }
        // if equal, nothing to emit
    }
}

impl<'src> Iterator for IndentLexer<'src> {
    type Item = SpannedToken;

    fn next(&mut self) -> Option<SpannedToken> {
        // Drain any pending synthetic tokens first (FIFO order)
        if !self.pending.is_empty() {
            return Some(self.pending.remove(0));
        }

        if self.finished {
            return None;
        }

        // If we've exhausted raw tokens, emit remaining Dedents and EOF
        if self.pos >= self.raw.len() {
            self.finished = true;
            let eof_span = self.source.len()..self.source.len();
            while self.indent_stack.len() > 1{
                self.indent_stack.pop();
                self.pending.push((Token::Dedent, eof_span.clone()));
            }
            self.pending.push((Token::Eof, eof_span));
            return if !self.pending.is_empty() {
                Some(self.pending.remove(0))
            } else {
                None
            };
        }

        let (token, span) = self.raw[self.pos].clone();
        self.pos += 1;

        match &token{
            // Track bracket depth to ignore indentation inside brackets
            Token::LParen | Token::LBracket | Token::LBrace => {
                self.bracket_depth += 1;
                Some((token, span))
            }
            Token::RParen | Token::RBracket | Token::RBrace => {
                if self.bracket_depth > 0 {
                    self.bracket_depth = self.bracket_depth.saturating_sub(1);
                }
                Some((token, span))
            }

            // Skip comment tokens entirely
            Token::Comment => self.next(),

            Token::Newline => {
                // Inside brackets, newlines don't affect indentation
                if self.bracket_depth > 0 {
                    return self.next();
                }

                // Skip consecutive blank lines / comment-only lines
                while self.pos < self.raw.len() {
                    match &self.raw[self.pos].0{
                        Token::Newline => {
                            self.pos += 1;
                        }
                        Token::Comment => {
                            self.pos += 1;
                        }
                        _ => break,
                    }
                }

                // Determine the byte offset where the next non-newline content starts
                // to measure its leading whitespace
                let line_start = if self.pos < self.raw.len() {
                    // We know the next token's span, so we can find the start of its line by looking backwards for a newline
                    let next_span_start = self.raw[self.pos].1.start;
                    // Find the last newline before next_span_start to get the start of the line
                    let line_begin = self.source[..next_span_start]
                        .rfind('\n')
                        .map(|idx| idx + 1)
                        .unwrap_or(0);
                    line_begin
                } else {
                    // EOF: treat as if we're at the end of the last line
                    self.source.len()
                };

                let new_indent = if self.pos < self.raw.len() {
                    self.measure_indent(line_start)
                } else {
                    0 // At EOF, we consider indentation to be 0 to close all blocks
                };

                // Emit the Newline, then queue any Indent/Dedent tokens
                self.handle_indent_change(new_indent, span.clone());
                Some((Token::Newline, span))
            }
            _ => Some((token, span)),
        }
    }
}