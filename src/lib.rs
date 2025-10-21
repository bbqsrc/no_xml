#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "jsx")]
pub mod jsx;

use core::{
    fmt::{Debug, Display},
    iter::Peekable,
    marker::PhantomData,
    ops::Range,
    str::CharIndices,
};

use collections2::ListMut;

#[cfg(not(feature = "std"))]
macro_rules! debug {
    ($($arg:tt)*) => {
        ()
    };
}

#[cfg(feature = "std")]
macro_rules! debug {
    ($($arg:tt)*) => {
        println!($($arg)*)
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WhiteSpace {
    Space,
    Tab,
    CarriageReturn,
    NewLine,
}

impl WhiteSpace {
    pub fn as_str(&self) -> &'static str {
        match self {
            WhiteSpace::Space => " ",
            WhiteSpace::Tab => "\t",
            WhiteSpace::CarriageReturn => "\r",
            WhiteSpace::NewLine => "\n",
        }
    }
}

impl TryFrom<char> for WhiteSpace {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            ' ' => Self::Space,
            '\t' => Self::Tab,
            '\r' => Self::CarriageReturn,
            '\n' => Self::NewLine,

            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal<'t> {
    EntityValue(&'t ()),
    AttValue,
    SystemLiteral,
    PubidLiteral,
    PubidChar,
    CharData,
    Comment,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Punc {
    TagStart,
    TagEnd,
    Slash,
    Ampersand,
    Percent,
    QuoteMark,
    Equals,
    QuestionMark,
    ExclamationMark,
    NumberSign,
    Colon,
    Semicolon,
    Dot,
    // Dash,
    Underscore,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Delim {
    CDataStart,
    CDataEnd,
    CommentStart,
    CommentEnd,
}

impl Delim {
    pub fn as_str(&self) -> &'static str {
        match self {
            Delim::CDataStart => "<![CDATA[",
            Delim::CDataEnd => "]]>",
            Delim::CommentStart => "<!--",
            Delim::CommentEnd => "-->",
        }
    }
}

impl Punc {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::TagStart => "<",
            Self::TagEnd => ">",
            Self::Slash => "/",
            Self::Ampersand => "&",
            Self::QuoteMark => "\"",
            Self::Equals => "=",
            Self::NumberSign => "#",
            Self::Percent => "%",
            Self::QuestionMark => "?",
            Self::ExclamationMark => "!",
            Self::Dot => ".",
            // Self::Dash => "-",
            Self::Underscore => "_",
            Self::Colon => ":",
            Self::Semicolon => ";",
        }
    }
}

impl TryFrom<&str> for Punc {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "<" => Self::TagStart,
            ">" => Self::TagEnd,
            "/" => Self::Slash,
            "&" => Self::Ampersand,
            "\"" => Self::QuoteMark,
            "=" => Self::Equals,
            "#" => Self::NumberSign,
            "%" => Self::Percent,
            "?" => Self::QuestionMark,
            "!" => Self::ExclamationMark,
            "." => Self::Dot,
            // "-" => Self::Dash,
            "_" => Self::Underscore,
            ":" => Self::Colon,
            ";" => Self::Semicolon,

            _ => return Err(()),
        })
    }
}

pub trait NonStandardToken: Debug + Clone + Sized {
    fn handle_char(_ch: char) -> Option<Self> {
        None
    }

    fn handle_str(_s: &str) -> Option<Self> {
        None
    }
}

impl NonStandardToken for () {}

#[derive(Debug, Clone)]
pub enum Token<'t, T = ()>
where
    T: NonStandardToken,
{
    NonStandard(T),

    Eof,
    Punc(Punc),
    Delim(Delim),
    Text(&'t str),
    WhiteSpace(WhiteSpace),
    Entity(&'t str),
}

impl<T> Token<'_, T>
where
    T: NonStandardToken,
{
    fn to_text(&self) -> Option<&str> {
        match self {
            Token::Punc(punc) => Some(punc.as_str()),
            Token::Text(t) => Some(*t),
            Token::WhiteSpace(ws) => Some(ws.as_str()),
            Token::Entity(e) => Some(*e),
            Token::Delim(_) => None,
            Token::NonStandard(_) => None,
            Token::Eof => None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Position {
    pub index: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Copy, Clone)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    fn as_range(&self) -> Range<usize> {
        self.start.index..self.end.index
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if f.alternate() {
            f.write_fmt(format_args!(
                "Ln {}, Col {} -> Ln {}, Col {}",
                self.start.line, self.start.col, self.end.line, self.end.col
            ))
        } else {
            f.write_fmt(format_args!(
                "{}:{}..{}:{}",
                self.start.line, self.start.col, self.end.line, self.end.col
            ))
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Span")
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}

pub type Spanned<T> = (Span, T);

#[derive(Debug)]
pub enum Error {
    // Utf8(core::str::Utf8Error),
    UnexpectedEof,
}

#[derive(Clone)]
pub struct Cursor<'a, T = ()> {
    start_pos: usize,
    start_line: usize,
    start_col: usize,
    end_pos: usize,
    end_line: usize,
    end_col: usize,
    char_count: usize,
    data: &'a str,
    chars: Peekable<CharIndices<'a>>,
    _ty: PhantomData<T>,
}

impl<T> Debug for Cursor<'_, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Cursor").finish_non_exhaustive()
    }
}

impl<'a, X> Cursor<'a, X>
where
    X: NonStandardToken,
{
    #[inline]
    pub fn new(buf: &'a str) -> Self {
        Self {
            start_pos: 0,
            start_line: 1,
            start_col: 1,
            end_pos: 0,
            end_line: 1,
            end_col: 1,
            char_count: 0,
            data: buf,
            chars: buf.char_indices().peekable(),
            _ty: PhantomData,
        }
    }

    #[inline]
    pub fn into_iter(self) -> Iter<'a, X> {
        Iter { cursor: Some(self) }
    }

    #[inline]
    fn consume(&mut self) -> Span {
        let s = self.span();
        self.start_pos = self.end_pos;
        self.start_col = self.end_col;
        self.start_line = self.end_line;
        self.char_count = 0;
        s
    }

    #[inline]
    fn peek_str(&self) -> &str {
        let r = self.start_pos..self.end_pos;
        if cfg!(debug_assertions) {
            self.data.get(r).expect("Invalid UTF-8 string")
        } else {
            &self.data[r]
        }
    }

    #[inline]
    fn peek_last_char(&self) -> Option<char> {
        self.peek_str().chars().next_back()
    }

    #[inline]
    fn peek_substr(&self, count: usize) -> &str {
        let s = self.peek_str();
        let mut indices = s.char_indices().take(count).map(|(i, _)| i);
        let end_index = indices.nth(count).unwrap_or(s.len());
        &s[0..end_index]
    }

    /// The length of the peekable buffer string in count of `char`s.
    #[inline]
    fn peek_char_count(&self) -> usize {
        self.char_count
    }

    #[inline]
    fn consume_with_span<T>(&mut self, input: T) -> (Span, T) {
        (self.consume(), input)
    }

    #[inline]
    fn consume_str(&mut self) -> (Span, &'a str) {
        let s = if cfg!(debug_assertions) {
            self.data
                .get(self.start_pos..self.end_pos)
                .expect("Invalid UTF-8 string")
        } else {
            &self.data[self.start_pos..self.end_pos]
        };

        (self.consume(), s)
    }

    #[inline]
    fn peek_next_char(&mut self) -> Option<char> {
        self.chars.peek().map(|x| x.1)
    }

    #[inline]
    fn peek_next_char_str(&mut self) -> Option<&str> {
        let ch = self.peek_next_char()?;
        let r = self.end_pos..self.end_pos + ch.len_utf8();
        Some(if cfg!(debug_assertions) {
            self.data.get(r).expect("Invalid UTF-8 string")
        } else {
            &self.data[r]
        })
    }

    #[inline]
    fn buffer_chars(&mut self, count: usize) {
        for i in 0..count {
            self.buffer_char();
        }
    }

    #[inline]
    fn buffer_chars_up_to(&mut self, count: usize) {
        self.buffer_chars(count.saturating_sub(self.char_count));
    }

    #[inline]
    fn buffer_char(&mut self) -> Option<char> {
        let (_, ch) = self.chars.next()?;
        match self.chars.peek() {
            Some((index, _)) => {
                self.end_pos = *index;
            }
            None => {
                self.end_pos = self.data.len();
            }
        }

        if ch == '\n' {
            self.end_line += 1;
            self.end_col = 1;
        } else {
            self.end_col += 1;
        }

        self.char_count += 1;

        Some(ch)
    }

    #[inline]
    fn span(&self) -> Span {
        Span {
            start: Position {
                index: self.start_pos,
                line: self.start_line,
                col: self.start_col,
            },
            end: Position {
                index: self.end_pos,
                line: self.end_line,
                col: self.end_col,
            },
        }
    }

    #[inline]
    fn with_span<T>(&self, input: T) -> Spanned<T> {
        (self.span(), input)
    }

    #[inline]
    fn err_with_span<T, E>(self, err: E) -> (Result<T, Spanned<E>>, Self) {
        let spanned = Err(self.with_span(err));
        (spanned, self)
    }

    #[inline]
    fn ok_with_span<T, E>(self, ok: T) -> (Result<Spanned<T>, E>, Self) {
        let spanned = Ok(self.with_span(ok));
        (spanned, self)
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.peek_str().is_empty()
    }
}

pub struct XmlVisitor;

impl Visitor for XmlVisitor {
    fn start_tag(&mut self, name: &str) {
        debug!("Start: {}", name);
    }

    fn self_closed_tag(&mut self, name: &str) {
        debug!("Self-closed: {name}");
    }

    fn end_tag(&mut self, name: &str) {
        debug!("End: {name}");
    }

    fn attribute(&mut self, key: &str, value: &str) {
        debug!("Attr: {key} => {value:?}")
    }

    fn text(&mut self, text: &str) {
        debug!("Text: {:?}", text);
    }

    fn comment(&mut self, text: &str) {
        debug!("Comment: {:?}", text);
    }

    fn entity(&mut self, entity: &str) {
        debug!("Entity: {:?}", entity);
    }
}

pub struct Iter<'t, T: NonStandardToken = ()> {
    cursor: Option<Cursor<'t, T>>,
}

impl<'t, T: NonStandardToken> Iterator for Iter<'t, T> {
    type Item = TokenResult<'t, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cursor) = self.cursor.take() {
            let (x, buf) = tokenize::<T>(cursor);
            match x {
                Ok((_, Token::Eof)) | Err(_) => {}
                _ => {
                    self.cursor = Some(buf);
                }
            }
            return Some(x);
        }

        None
    }
}

pub type TokenResult<'t, T = ()> = Result<Spanned<Token<'t, T>>, Spanned<Error>>;
pub type Maybe<T> = Option<Spanned<T>>;

fn delim<'t, T: NonStandardToken>(base_buf: Cursor<'t, T>) -> (Maybe<Delim>, Cursor<'t, T>) {
    let mut buf = base_buf.clone();
    buf.buffer_chars_up_to(3);
    let cur = buf.peek_substr(3);

    match cur {
        "-->" => return (Some(buf.consume_with_span(Delim::CommentEnd)), buf),
        "]]>" => return (Some(buf.consume_with_span(Delim::CDataEnd)), buf),

        _ => {}
    }

    buf.buffer_chars_up_to(4);
    let cur = buf.peek_substr(4);

    if cur == "<!--" {
        return (Some(buf.consume_with_span(Delim::CommentStart)), buf);
    }

    buf.buffer_chars_up_to(9);
    let cur = buf.peek_substr(9);

    if cur == "<![CDATA[" {
        return (Some(buf.consume_with_span(Delim::CDataStart)), buf);
    }

    (None, base_buf)
}

fn tokenize<'t, T>(mut buf: Cursor<'t, T>) -> (TokenResult<'t, T>, Cursor<'t, T>)
where
    T: NonStandardToken,
{
    if !buf.is_empty() {
        if let Some(chs) = buf.peek_next_char_str() {
            let ch = chs.chars().next().unwrap();
            if WhiteSpace::try_from(ch).is_ok()
                || Punc::try_from(chs).is_ok()
                || T::handle_char(ch).is_some()
            {
                if let Some(non_standard) = T::handle_str(buf.peek_str()) {
                    return (
                        Ok(buf.consume_with_span(Token::NonStandard(non_standard))),
                        buf,
                    );
                }

                let (span, s) = buf.consume_str();
                return (Ok((span, Token::Text(s))), buf);
            }
        } else {
            let (span, s) = buf.consume_str();
            return (Ok((span, Token::Text(s))), buf);
        }
    }

    if buf.buffer_char().is_none() {
        if !buf.is_empty() {
            return buf.err_with_span(Error::UnexpectedEof);
        }

        return buf.ok_with_span(Token::Eof);
    }

    let ch = buf.peek_str().chars().next().unwrap();
    if let Ok(ws) = WhiteSpace::try_from(ch) {
        let span = buf.consume();
        return (Ok((span, Token::WhiteSpace(ws))), buf);
    }

    let (delim, mut buf) = delim(buf);
    if let Some((span, delim)) = delim {
        return (Ok((span, Token::Delim(delim))), buf);
    }

    if let Ok(punc) = Punc::try_from(buf.peek_str()) {
        let span = buf.consume();
        return (Ok((span, Token::Punc(punc))), buf);
    }

    if let Some(non_standard) = T::handle_char(ch) {
        let span = buf.consume();
        return (Ok((span, Token::NonStandard(non_standard))), buf);
    }

    tokenize(buf)
}

pub enum Event<'e> {
    StartTag { name: &'e str },
    EndTag { name: &'e str },
}

#[derive(Debug, Copy, Clone)]
pub enum State<'s, T = ()>
where
    T: Debug + Copy,
{
    NonStandard(T),

    None,
    StartTagExpectingName,
    StartTagExpectingAttrs,
    StartTagExpectingGt,
    EndTagExpectingName,
    EndTagExpectingGt(&'s str),
    AttrExpectingEqOrNamespace(&'s str),
    AttrExpectingOpeningQuote(&'s str),
    AttrExpectingValue(&'s str),
    AttrExpectingClosingQuote(&'s str, &'s str),
    EntityExpectingValue,
    EntityExpectingNumerals,
    EntityExpectingSemicolon(&'s str),
    // DeclExpectingName,
}

#[derive(Debug, Copy, Clone)]
pub enum StateMode {
    None,
    CData,
    Comment,
}

pub trait NonStandardEventHandler<'s>: Default + Copy {
    type Token: NonStandardToken;
    type State: Debug + Copy;
    type Visitor: Visitor;

    fn process_state(
        &mut self,
        span: Span,
        token: Token<'s, Self::Token>,
        state: State<'s, Self::State>,
        path: &mut impl ListMut<&'s str>,
        visitor: &mut Self::Visitor,
        raw_input: &'s str,
    ) -> State<'s, Self::State>;
}

impl<'s> NonStandardEventHandler<'s> for () {
    type Token = ();
    type State = ();
    type Visitor = XmlVisitor;

    fn process_state(
        &mut self,
        _span: Span,
        _token: Token<'s, Self::Token>,
        state: State<'s, Self::State>,
        _path: &mut impl ListMut<&'s str>,
        _visitor: &mut Self::Visitor,
        _raw_input: &'s str,
    ) -> State<'s, Self::State> {
        state
    }
}

pub trait Visitor {
    fn start_tag(&mut self, name: &str);
    fn self_closed_tag(&mut self, name: &str);
    fn end_tag(&mut self, name: &str);
    fn attribute(&mut self, key: &str, value: &str);
    fn text(&mut self, text: &str);
    fn comment(&mut self, text: &str);
    fn entity(&mut self, entity: &str);
}

pub fn stream_events<'s, S, V>(
    input: &'s str,
    visitor: &mut S::Visitor,
) -> Result<(), Spanned<Error>>
where
    S: NonStandardEventHandler<'s>,
    V: ListMut<&'s str> + Default + Debug,
{
    let mut buf = Cursor::<'_, S::Token>::new(input);
    let mut path = V::default();
    let mut state: State<S::State> = State::None;
    let mut state_mode = StateMode::None;
    let mut non_standard_handler = S::default();

    loop {
        let (item, b) = tokenize(buf);
        buf = b;
        let (span, token) = item?;

        // debug!("Token: {:?} at {span}", token);

        if matches!(token, Token::Eof) {
            // debug!("EOF reached");
            break;
        }

        // debug!("[state:1] {state:?} {path:?}");
        state = non_standard_handler.process_state(
            span,
            token.clone(),
            state,
            &mut path,
            visitor,
            input,
        );

        // debug!("[state:2] {state:?}");

        if matches!(state_mode, StateMode::CData) {
            if let Token::Delim(Delim::CDataEnd) = token {
                state_mode = StateMode::None;
                continue;
            }

            if let Some(text) = token.to_text() {
                visitor.text(text);
            } else {
                panic!("Invalid token in CDATA: {:?}", token);
            }

            continue;
        }

        if matches!(state_mode, StateMode::Comment) {
            if let Token::Delim(Delim::CommentEnd) = token {
                state_mode = StateMode::None;
            } else if let Token::Text(text) = token {
                visitor.comment(text);
            } else {
                panic!("Invalid token in Comment: {:?}", token);
            }

            continue;
        }

        match state {
            State::NonStandard(_) => { /* Always a no-op */ }
            State::None => {
                if matches!(token, Token::Punc(Punc::Ampersand)) {
                    state = State::EntityExpectingValue;
                    continue;
                }

                if matches!(token, Token::Punc(Punc::TagStart)) {
                    let (item, _) = tokenize(buf.clone());
                    let (_, token2) = item?;

                    if matches!(token2, Token::Punc(Punc::Slash)) {
                        state = State::EndTagExpectingName;
                        continue;
                    }

                    state = State::StartTagExpectingName;
                    continue;
                }

                match token {
                    Token::NonStandard(_) => todo!("non-standard"),
                    Token::Eof => todo!("eof"),
                    Token::Punc(punc) => visitor.text(punc.as_str()),
                    Token::Delim(val) => match val {
                        Delim::CDataStart => {
                            state_mode = StateMode::CData;
                            continue;
                        }
                        Delim::CDataEnd => {}
                        Delim::CommentStart => {}
                        Delim::CommentEnd => {}
                    },
                    Token::Text(text) => visitor.text(text),
                    Token::WhiteSpace(ws) => visitor.text(ws.as_str()),
                    Token::Entity(_) => todo!("entity"),
                }
            }
            State::StartTagExpectingName => {
                if matches!(token, Token::WhiteSpace(_)) {
                    continue;
                }

                // if matches!(token, Token::Punc(Punc::ExclamationMark)) {
                //     state = State::DeclExpectingName;
                //     continue;
                // }

                if let Token::Text(name) = token {
                    path.push(name);

                    visitor.start_tag(name);

                    state = State::StartTagExpectingAttrs;
                    continue;
                }

                panic!("Name not found! {:?}", token);
            }
            // State::DeclExpectingName => {
            //     if matches!(token, Token::WhiteSpace(_)) {
            //         continue;
            //     }

            //     if let Token::Text(decl) = token {
            //         match decl {
            //             "CDATA" => {

            //                 let (item, b) = tokenize(buf);
            //                 let (_span, token1) = item?;

            //                 let (item2, b) = tokenize(b);
            //                 let (_span, token2) = item2?;

            //                 if matches!(token1, Token::Delim(Delim::CDataStart))
            //                     && matches!(token2, Token::Punc(Punc::TagEnd))
            //                 {
            //                     // debug!("CDATA start");
            //                     state = State::None;

            //                 buf = b;
            //                 continue;
            //             }
            //             decl => {
            //                 panic!("Unknown decl type: {:?}", decl);
            //             }
            //         }
            //         // path.push(name);

            //         // visitor.start_tag(name);

            //         state = State::StartTagExpectingAttrs;
            //         continue;
            //     }

            //     panic!("Decl name not found! {:?}", token);
            // }
            State::StartTagExpectingAttrs => {
                if matches!(token, Token::Punc(Punc::Slash)) {
                    state = State::StartTagExpectingGt;
                    continue;
                }

                if matches!(token, Token::Punc(Punc::TagEnd)) {
                    state = State::None;
                }

                if let Token::Text(text) = token {
                    state = State::AttrExpectingEqOrNamespace(text);
                }
            }
            State::StartTagExpectingGt => {
                if matches!(token, Token::Punc(Punc::TagEnd)) {
                    let name = path.pop().unwrap();

                    visitor.self_closed_tag(name);
                    // debug!("Self-closed tag: {}", name);

                    state = State::None;
                    continue;
                } else {
                    panic!("Self-closing tag did not close properly");
                }
            }
            State::EndTagExpectingGt(name) => {
                if matches!(token, Token::Punc(Punc::TagEnd)) {
                    visitor.end_tag(name);

                    state = State::None;
                    continue;
                } else {
                    panic!("Self-closing tag did not close properly");
                }
            }
            State::EndTagExpectingName => {
                if matches!(token, Token::WhiteSpace(_)) {
                    continue;
                }

                if let Token::Text(name) = token {
                    if let Some(tag) = path.pop() {
                        if tag != name {
                            panic!(
                                "Invalid closing tag. Encountered: {:?}, expected: {:?}",
                                tag, name
                            );
                        }

                        // debug!("End tag: {}", name);
                        state = State::EndTagExpectingGt(name);
                    } else {
                        panic!("Tried to close emptiness");
                    }
                }
            }
            State::AttrExpectingEqOrNamespace(key) => {
                if matches!(token, Token::Punc(Punc::Equals)) {
                    state = State::AttrExpectingOpeningQuote(key);
                } else if matches!(token, Token::Punc(Punc::Colon)) {
                    panic!("Namespace support incomplete");
                } else {
                    panic!("No equals for attr")
                }
            }
            State::AttrExpectingValue(key) => {
                if let Token::Text(value) = token {
                    state = State::AttrExpectingClosingQuote(key, value);
                } else if matches!(token, Token::Punc(Punc::QuoteMark)) {
                    visitor.attribute(key, "");
                    // debug!("Attr: {key} => \"\"");
                    state = State::StartTagExpectingAttrs;
                } else {
                    panic!("No value! {:?}", token);
                }
            }
            State::AttrExpectingOpeningQuote(key) => {
                if matches!(token, Token::Punc(Punc::QuoteMark)) {
                    state = State::AttrExpectingValue(key);
                } else {
                    panic!("Invalid attr, no quote");
                }
            }
            State::AttrExpectingClosingQuote(key, value) => {
                if matches!(token, Token::Punc(Punc::QuoteMark)) {
                    visitor.attribute(key, value);
                    // debug!("Attr: {key} => {value:?}");
                    state = State::StartTagExpectingAttrs;
                } else {
                    panic!("Invalid attr, no quote");
                }
            }
            State::EntityExpectingValue => match token {
                Token::NonStandard(_) => todo!("non-standard"),
                Token::Eof => todo!("eof"),
                Token::Punc(Punc::NumberSign) => {
                    state = State::EntityExpectingNumerals;
                }
                Token::Punc(punc) => todo!("unepxected punc: {punc:?}"),
                Token::Delim(_) => todo!("delim {state:?}"),
                Token::Text(text) => {
                    state = State::EntityExpectingSemicolon(text);
                }
                Token::WhiteSpace(_) => todo!("whitespace"),
                Token::Entity(_) => todo!("entity"),
            },
            State::EntityExpectingNumerals => match token {
                Token::NonStandard(_) => todo!("non-standard"),
                Token::Eof => todo!("eof"),
                Token::Punc(punc) => todo!("unepxected punc: {punc:?}"),
                Token::Delim(_) => todo!("delim {state:?}"),
                Token::Text(text) => {
                    state = State::EntityExpectingSemicolon(text);
                }
                Token::WhiteSpace(_) => todo!("whitespace"),
                Token::Entity(_) => todo!("entity"),
            },
            State::EntityExpectingSemicolon(value) => {
                if matches!(token, Token::Punc(Punc::Semicolon)) {
                    visitor.entity(value);
                    state = State::None;
                } else {
                    panic!("Invalid entity!")
                }
            }
        }
    }

    debug!("Done parsing XML");

    Ok(())
}

#[cfg(not(feature = "std"))]
type Vec<T> = heapless::Vec<T, 256>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Cursor, stream_events};

    #[test]
    fn test() {
        let mut x = Cursor::<'_, ()>::new("abcdefghijk");
        debug!("{:?}", x.peek_str());
        x.buffer_char();
        debug!("{:?}", x.peek_str());
        x.buffer_char();
        debug!("{:?}", x.peek_str());
        x.consume();
        debug!("{:?}", x.peek_str());
        x.buffer_char();
        x.buffer_char();
        x.buffer_char();
        x.buffer_char();
        debug!("{:?}", x.peek_str());

        let xemel = "<root>
        <a thing=\"foo\" />< a ></
        a> &#0a00;&amp;
        <b/><c  attr=\"lol\"><d>lol<!-- this is a comment
        still a comment
        another
        -->
        </d>dangler = | : / <![CDATA[this is &raw;]]>
    </c>
    </root>AWOO";

        let x = Cursor::<'_, ()>::new(xemel);
        debug!("{:?}", x.into_iter().collect::<Result<Vec<_>, _>>());

        stream_events::<(), Vec<&'_ str>>(xemel, &mut XmlVisitor).unwrap();
        debug!();
        stream_events::<(), Vec<&'_ str>>("&#00a0;", &mut XmlVisitor).unwrap();
    }
}
