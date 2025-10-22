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
use heapless::index_map::Values;

const MAX_DOCTYPE_DEPTH: u8 = 6;

#[cfg(not(feature = "std"))]
macro_rules! debug {
    ($($arg:tt)*) => {
        ()
    };
}

#[cfg(feature = "std")]
macro_rules! debug {
    ($($arg:tt)*) => {
        // println!($($arg)*)
        ()
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
    LeftSquareBracket,
    RightSquareBracket,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Delim {
    CDataStart,
    CDataEnd,
    CommentStart,
    CommentEnd,
    ProcessingInstructionStart,
    ProcessingInstructionEnd,
    Doctype,
    Entity,
    Element,
}

impl Delim {
    pub fn as_str(&self) -> &'static str {
        match self {
            Delim::CDataStart => "<![CDATA[",
            Delim::CDataEnd => "]]>",
            Delim::CommentStart => "<!--",
            Delim::CommentEnd => "-->",
            Delim::ProcessingInstructionStart => "<?",
            Delim::ProcessingInstructionEnd => "?>",
            Delim::Doctype => "<!DOCTYPE",
            Delim::Entity => "<!ENTITY",
            Delim::Element => "<!ELEMENT",
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
            Self::LeftSquareBracket => "[",
            Self::RightSquareBracket => "]",
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
            "[" => Self::LeftSquareBracket,
            "]" => Self::RightSquareBracket,

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
    EntityRef(&'t str),
}

impl<'t, T> Token<'t, T>
where
    T: NonStandardToken,
{
    fn to_text(&self) -> Option<&'t str> {
        match self {
            Token::Punc(punc) => Some(punc.as_str()),
            Token::Text(t) => Some(*t),
            Token::WhiteSpace(ws) => Some(ws.as_str()),
            Token::EntityRef(e) => Some(*e),
            Token::Delim(delim) => Some(delim.as_str()),
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

#[derive(Debug, Clone)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    UnexpectedEof,
    MismatchedTag,
    InvalidClosingTag,
    InvalidAttributeSyntax,
    InvalidEntity,
    SelfClosingTagNotClosed,
    UnexpectedToken,
    TagNameExpected,
    UnimplementedFeature,
    DoctypeNestingTooDeep,
}

impl core::error::Error for Error {}
impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.kind {
            ErrorKind::UnexpectedEof => write!(f, "Unexpected end of file")?,
            ErrorKind::MismatchedTag => write!(f, "Mismatched tag")?,
            ErrorKind::InvalidClosingTag => write!(f, "Invalid closing tag")?,
            ErrorKind::InvalidAttributeSyntax => write!(f, "Invalid attribute syntax")?,
            ErrorKind::InvalidEntity => write!(f, "Invalid entity")?,
            ErrorKind::SelfClosingTagNotClosed => write!(f, "Self-closing tag not closed")?,
            ErrorKind::UnexpectedToken => write!(f, "Unexpected token")?,
            ErrorKind::TagNameExpected => write!(f, "Tag name expected")?,
            ErrorKind::UnimplementedFeature => write!(f, "Unimplemented feature")?,
            ErrorKind::DoctypeNestingTooDeep => write!(f, "DOCTYPE nesting too deep")?,
        }
        write!(f, " ({})", self.span)
    }
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
        for _ in 0..count {
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
    fn with_span<T>(self, input: T) -> (Span, T, Cursor<'a, X>) {
        (self.span(), input, self)
    }

    #[inline]
    fn err_with_span<T>(self, err: ErrorKind) -> Result<T, Error> {
        Err(Error {
            span: self.span(),
            kind: err,
        })
    }

    #[inline]
    fn ok_with_span<T>(self, ok: T) -> Result<(Span, T, Cursor<'a, X>), Error> {
        Ok(self.with_span(ok))
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.peek_str().is_empty()
    }
}

pub struct XmlVisitor;

impl Visitor for XmlVisitor {
    fn start_tag(&mut self, name: &str) -> Result<(), Error> {
        debug!("Start: {}", name);
        Ok(())
    }

    fn end_tag(&mut self, name: &str) -> Result<(), Error> {
        debug!("End: {name}");
        Ok(())
    }

    fn attribute(&mut self, key: &str, value: &str) -> Result<(), Error> {
        debug!("Attr: {key} => {value:?}");
        Ok(())
    }

    fn text(&mut self, text: &str) -> Result<(), Error> {
        debug!("Text: {:?}", text);
        Ok(())
    }

    fn comment(&mut self, text: &str) -> Result<(), Error> {
        debug!("Comment: {:?}", text);
        Ok(())
    }

    fn start_comment(&mut self) -> Result<(), Error> {
        debug!("Start comment");
        Ok(())
    }

    fn end_comment(&mut self) -> Result<(), Error> {
        debug!("End comment");
        Ok(())
    }

    fn entity_ref(&mut self, entity: &str) -> Result<(), Error> {
        debug!("Entity: {:?}", entity);
        Ok(())
    }

    fn start_processing_instruction(&mut self) -> Result<(), Error> {
        debug!("Start PI");
        Ok(())
    }

    fn processing_instruction(&mut self, content: &str) -> Result<(), Error> {
        debug!("Processing instruction: {}", content);
        Ok(())
    }

    fn end_processing_instruction(&mut self) -> Result<(), Error> {
        debug!("End PI");
        Ok(())
    }

    fn start_doctype(&mut self) -> Result<(), Error> {
        debug!("Start DOCTYPE");
        Ok(())
    }

    fn doctype(&mut self, content: &str) -> Result<(), Error> {
        debug!("DOCTYPE: {:?}", content);
        Ok(())
    }

    fn end_doctype(&mut self) -> Result<(), Error> {
        debug!("End DOCTYPE");
        Ok(())
    }

    fn end_entity_declaration(&mut self) -> Result<(), Error> {
        debug!("End ENTITY");
        Ok(())
    }

    fn start_entity_declaration(&mut self) -> Result<(), Error> {
        debug!("Start ENTITY");
        Ok(())
    }
    fn entity_declaration(&mut self, content: &str) -> Result<(), Error> {
        debug!("{}", content);
        Ok(())
    }

    fn element_declaration(&mut self, _content: &str) -> Result<(), Error> {
        debug!("ELEMENT {_content}");
        Ok(())
    }

    fn end_element_declaration(&mut self) -> Result<(), Error> {
        debug!("End ELEMENT");
        Ok(())
    }

    fn start_element_declaration(&mut self) -> Result<(), Error> {
        debug!("Start ELEMENT");
        Ok(())
    }
}

pub struct Iter<'t, T: NonStandardToken = ()> {
    cursor: Option<Cursor<'t, T>>,
}

impl<'t, T: NonStandardToken> Iterator for Iter<'t, T> {
    type Item = (Span, Token<'t, T>, Cursor<'t, T>);

    fn next(&mut self) -> Option<Self::Item> {
        let Some(cursor) = self.cursor.take() else {
            return None;
        };

        match tokenize::<T>(cursor, StateMode::None) {
            Err(_) | Ok((_, Token::Eof, _)) => None,
            Ok(tup) => Some(tup),
        }
    }
}

// pub type TokenResult<'t, T = ()> = Result<Spanned<Token<'t, T>>, Spanned<Error>>;
pub type Maybe<T> = Option<Spanned<T>>;

#[inline]
fn delim_directive<'t, T: NonStandardToken>(
    base_buf: Cursor<'t, T>,
) -> (Maybe<Delim>, Cursor<'t, T>) {
    let mut buf = base_buf.clone();

    buf.buffer_chars_up_to(4);
    let cur = buf.peek_substr(4);

    if cur == "<!--" {
        return (Some(buf.consume_with_span(Delim::CommentStart)), buf);
    }

    buf.buffer_chars_up_to(8);
    let cur = buf.peek_substr(8);

    if cur == "<!ENTITY" {
        return (Some(buf.consume_with_span(Delim::Entity)), buf);
    }

    buf.buffer_chars_up_to(9);
    let cur = buf.peek_substr(9);

    if cur == "<!DOCTYPE" {
        return (Some(buf.consume_with_span(Delim::Doctype)), buf);
    }

    if cur == "<![CDATA[" {
        return (Some(buf.consume_with_span(Delim::CDataStart)), buf);
    }

    if cur == "<!ELEMENT" {
        return (Some(buf.consume_with_span(Delim::Element)), buf);
    }

    return (None, base_buf);
}

fn delim<'t, T: NonStandardToken>(
    base_buf: Cursor<'t, T>,
    state_mode: StateMode,
) -> (Maybe<Delim>, Cursor<'t, T>) {
    let mut buf = base_buf.clone();
    buf.buffer_chars_up_to(2);
    let cur = buf.peek_substr(2);

    match cur {
        "<!" => return delim_directive(buf),
        "<?" => {
            return (
                Some(buf.consume_with_span(Delim::ProcessingInstructionStart)),
                buf,
            );
        }
        "?>" => {
            return (
                Some(buf.consume_with_span(Delim::ProcessingInstructionEnd)),
                buf,
            );
        }
        _ => {}
    }

    buf.buffer_chars_up_to(3);
    let cur = buf.peek_substr(3);

    match cur {
        "-->" => return (Some(buf.consume_with_span(Delim::CommentEnd)), buf),
        // This is hacky, the tokenizer shouldn't need to be stateful
        "]]>" if matches!(state_mode, StateMode::CData) => {
            return (Some(buf.consume_with_span(Delim::CDataEnd)), buf);
        }

        _ => {}
    }

    (None, base_buf)
}

fn tokenize<'t, T>(
    mut buf: Cursor<'t, T>,
    state_mode: StateMode,
) -> Result<(Span, Token<'t, T>, Cursor<'t, T>), Error>
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
                    let (span, token) = buf.consume_with_span(Token::NonStandard(non_standard));
                    return Ok((span, token, buf));
                }

                let (span, s) = buf.consume_str();
                return Ok((span, Token::Text(s), buf));
            }
        } else {
            let (span, s) = buf.consume_str();
            return Ok((span, Token::Text(s), buf));
        }
    }

    if buf.buffer_char().is_none() {
        if !buf.is_empty() {
            return buf.err_with_span(ErrorKind::UnexpectedEof);
        }

        return buf.ok_with_span(Token::Eof);
    }

    let ch = buf.peek_str().chars().next().unwrap();
    if let Ok(ws) = WhiteSpace::try_from(ch) {
        let span = buf.consume();
        return Ok((span, Token::WhiteSpace(ws), buf));
    }

    let (delim, mut buf) = delim(buf, state_mode);
    if let Some((span, delim)) = delim {
        return Ok((span, Token::Delim(delim), buf));
    }

    if let Ok(punc) = Punc::try_from(buf.peek_str()) {
        let span = buf.consume();
        return Ok((span, Token::Punc(punc), buf));
    }

    if let Some(non_standard) = T::handle_char(ch) {
        let span = buf.consume();
        return Ok((span, Token::NonStandard(non_standard), buf));
    }

    tokenize(buf, state_mode)
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
    AttrExpectingEqOrNamespace(&'s str, Position),
    AttrExpectingNamespaceLocalName(&'s str, Position),
    AttrExpectingOpeningQuote(&'s str),
    AttrExpectingValue(&'s str, Position),
    AttrExpectingClosingQuote(&'s str, Position),
    EntityRefExpectingValue(Position),
    EntityRefExpectingNumerals(Position),
    EntityRefExpectingSemicolon(Position),
    EntityDeclExpectingContent,
    ElementDeclExpectingContent,
}

#[derive(Debug, Copy, Clone)]
pub enum StateMode {
    None,
    CData,
    Comment,
    ProcessingInstruction,
    Doctype,
    DoctypeInner(u8),
}

// pub trait NonStandardEventHandler<'s>: Default + Copy {
//     type Token: NonStandardToken;
//     type State: Debug + Copy;
//     type Visitor: Visitor;

//     fn process_state(
//         &mut self,
//         span: Span,
//         token: Token<'s, Self::Token>,
//         state: State<'s, Self::State>,
//         path: &mut impl ListMut<&'s str>,
//         visitor: &mut Self::Visitor,
//         raw_input: &'s str,
//     ) -> Result<State<'s, Self::State>, Error>;
// }

// impl<'s> NonStandardEventHandler<'s> for () {
//     type Token = ();
//     type State = ();
//     type Visitor = XmlVisitor;

//     fn process_state(
//         &mut self,
//         _span: Span,
//         _token: Token<'s, Self::Token>,
//         state: State<'s, Self::State>,
//         _path: &mut impl ListMut<&'s str>,
//         _visitor: &mut Self::Visitor,
//         _raw_input: &'s str,
//     ) -> Result<State<'s, Self::State>, Error> {
//         Ok(state)
//     }
// }

pub trait Parser<V: Visitor>: Default {
    type Token: NonStandardToken;
    type State<'s>: Debug + Copy;

    fn process_state<'s>(
        &mut self,
        _span: Span,
        _token: Token<'s, Self::Token>,
        state: State<'s, Self::State<'s>>,
        _path: &mut impl ListMut<&'s str>,
        _visitor: &mut V,
        _raw_input: &'s str,
    ) -> Result<State<'s, Self::State<'s>>, Error> {
        Ok(state)
    }
}

#[derive(Debug, Default)]
pub struct XmlParser;

impl<V: Visitor> Parser<V> for XmlParser {
    type Token = ();
    type State<'s> = ();
}

pub trait Visitor {
    fn start_tag(&mut self, _name: &str) -> Result<(), Error> {
        Ok(())
    }
    fn end_tag(&mut self, _name: &str) -> Result<(), Error> {
        Ok(())
    }
    fn attribute(&mut self, _key: &str, _value: &str) -> Result<(), Error> {
        Ok(())
    }
    fn text(&mut self, _text: &str) -> Result<(), Error> {
        Ok(())
    }
    fn comment(&mut self, _text: &str) -> Result<(), Error> {
        Ok(())
    }
    fn start_comment(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn end_comment(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn entity_ref(&mut self, _entity_ref: &str) -> Result<(), Error> {
        Ok(())
    }
    fn start_processing_instruction(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn processing_instruction(&mut self, _content: &str) -> Result<(), Error> {
        Ok(())
    }
    fn end_processing_instruction(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn start_doctype(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn doctype(&mut self, _content: &str) -> Result<(), Error> {
        Ok(())
    }
    fn end_doctype(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn start_entity_declaration(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn end_entity_declaration(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn entity_declaration(&mut self, _content: &str) -> Result<(), Error> {
        Ok(())
    }
    fn start_element_declaration(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn end_element_declaration(&mut self) -> Result<(), Error> {
        Ok(())
    }
    fn element_declaration(&mut self, _content: &str) -> Result<(), Error> {
        Ok(())
    }
}

fn parse_doctype_header(content: &str) -> (Option<&str>, Option<&str>) {
    let content = content.trim();
    if content.is_empty() {
        return (None, None);
    }

    let mut parts = content.splitn(2, |c: char| c.is_whitespace());
    let name = parts.next().filter(|s| !s.is_empty());
    let external_id = parts
        .next()
        .filter(|s| !s.trim().is_empty())
        .map(|s| s.trim());

    (name, external_id)
}

fn split_doctype_content(full_content: &str, subset_offset: usize) -> (&str, &str) {
    let header_end = subset_offset.saturating_sub(1);
    let header = full_content[..header_end].trim();

    let subset_start = subset_offset;
    let subset_content = &full_content[subset_start..];

    if let Some(end_bracket) = subset_content.rfind(']') {
        let internal_subset = subset_content[..end_bracket].trim();
        (header, internal_subset)
    } else {
        (header, subset_content.trim())
    }
}

pub fn stream_xml_events<'s, L>(input: &'s str, visitor: &mut impl Visitor) -> Result<(), Error>
where
    L: ListMut<&'s str> + Default + Debug,
{
    stream_events::<L, XmlParser, _>(input, visitor)
}

pub fn stream_events<'s, L, P, V>(input: &'s str, visitor: &mut V) -> Result<(), Error>
where
    V: Visitor,
    P: Parser<V> + Default,
    L: ListMut<&'s str> + Default + Debug,
{
    let mut buf = Cursor::<'_, P::Token>::new(input);
    let mut path = L::default();
    let mut state: State<P::State<'s>> = State::None;
    let mut state_mode = StateMode::None;
    let mut parser = P::default();

    loop {
        let (span, token, b) = tokenize(buf, state_mode)?;
        // println!("Token: {:?} at {span}", token);
        buf = b;

        if matches!(token, Token::Eof) {
            // debug!("EOF reached");
            break;
        }

        // debug!("[state:1] {state:?} {path:?}");
        state = parser.process_state(span, token.clone(), state, &mut path, visitor, input)?;

        // debug!("[state:2] {state:?}");

        if matches!(state_mode, StateMode::Doctype) {
            if matches!(token, Token::Punc(Punc::TagEnd)) {
                visitor.end_doctype()?;
                state_mode = StateMode::None;
                continue;
            }

            if matches!(token, Token::Punc(Punc::LeftSquareBracket)) {
                state_mode = StateMode::DoctypeInner(0);
                continue;
            }

            if matches!(token, Token::WhiteSpace(_)) {
                continue;
            }

            if let Some(content) = token.to_text() {
                visitor.doctype(content)?;
            } else {
                return Err(Error {
                    span,
                    kind: ErrorKind::UnexpectedToken,
                });
            }

            continue;
        }

        if let StateMode::DoctypeInner(depth) = state_mode {
            if matches!(token, Token::Punc(Punc::RightSquareBracket)) {
                if depth == 0 {
                    state_mode = StateMode::Doctype;
                } else {
                    state_mode = StateMode::DoctypeInner(depth - 1);
                }

                continue;
            }

            if matches!(token, Token::WhiteSpace(_)) {
                continue;
            }

            // Fall through
        }

        if matches!(state_mode, StateMode::CData) {
            if let Token::Delim(Delim::CDataEnd) = token {
                state_mode = StateMode::None;
                continue;
            }

            if let Some(text) = token.to_text() {
                visitor.text(text)?;
            } else {
                return Err(Error {
                    span,
                    kind: ErrorKind::UnexpectedToken,
                });
            }

            continue;
        }

        if matches!(state_mode, StateMode::Comment) {
            if let Token::Delim(Delim::CommentEnd) = token {
                visitor.end_comment()?;
                state_mode = StateMode::None;
            }

            if let Some(text) = token.to_text() {
                visitor.comment(text)?;
            } else {
                return Err(Error {
                    span,
                    kind: ErrorKind::UnexpectedToken,
                });
            }

            continue;
        }

        if matches!(state_mode, StateMode::ProcessingInstruction) {
            if let Token::Delim(Delim::ProcessingInstructionEnd) = token {
                visitor.end_processing_instruction()?;
                state_mode = StateMode::None;
                continue;
            }

            if let Some(text) = token.to_text() {
                visitor.processing_instruction(text)?;
            } else {
                return Err(Error {
                    span,
                    kind: ErrorKind::UnexpectedToken,
                });
            }

            continue;
        }

        match state {
            State::NonStandard(_) => { /* Always a no-op */ }
            State::None => {
                if matches!(token, Token::Punc(Punc::Ampersand)) {
                    state = State::EntityRefExpectingValue(span.end);
                    continue;
                }

                if matches!(token, Token::Punc(Punc::TagStart)) {
                    let (_, token2, _) = tokenize(buf.clone(), state_mode)?;

                    if matches!(token2, Token::Punc(Punc::Slash)) {
                        state = State::EndTagExpectingName;
                        continue;
                    }

                    state = State::StartTagExpectingName;
                    continue;
                }

                match token {
                    Token::NonStandard(_) => {
                        return Err(Error {
                            span,
                            kind: ErrorKind::UnexpectedToken,
                        });
                    }
                    Token::Eof => {
                        return Err(Error {
                            span,
                            kind: ErrorKind::UnexpectedEof,
                        });
                    }
                    Token::Punc(punc) => visitor.text(punc.as_str())?,
                    Token::Delim(val) => match val {
                        Delim::CDataStart => {
                            state_mode = StateMode::CData;
                            continue;
                        }
                        Delim::CommentStart => {
                            state_mode = StateMode::Comment;
                            visitor.start_comment()?;
                            continue;
                        }
                        Delim::ProcessingInstructionStart => {
                            state_mode = StateMode::ProcessingInstruction;
                            visitor.start_processing_instruction()?;
                            continue;
                        }
                        Delim::Doctype => {
                            state_mode = StateMode::Doctype;
                            visitor.start_doctype()?;
                            continue;
                        }
                        Delim::Entity => {
                            state = State::EntityDeclExpectingContent;
                            visitor.start_entity_declaration()?;
                            continue;
                        }
                        Delim::Element => {
                            state = State::ElementDeclExpectingContent;
                            visitor.start_element_declaration()?;
                            continue;
                        }
                        _ => {}
                    },
                    Token::Text(text) => visitor.text(text)?,
                    Token::WhiteSpace(ws) => visitor.text(ws.as_str())?,
                    Token::EntityRef(entity) => visitor.entity_ref(entity)?,
                }

                continue;
            }
            State::StartTagExpectingName => {
                if matches!(token, Token::WhiteSpace(_)) {
                    continue;
                }

                if let Some(name) = token.to_text() {
                    path.push(name);

                    visitor.start_tag(name)?;

                    state = State::StartTagExpectingAttrs;
                    continue;
                }

                return Err(Error {
                    span,
                    kind: ErrorKind::TagNameExpected,
                });
            }
            State::StartTagExpectingAttrs => {
                if matches!(token, Token::Punc(Punc::Slash)) {
                    state = State::StartTagExpectingGt;
                    continue;
                }

                if matches!(token, Token::Punc(Punc::TagEnd)) {
                    state = State::None;
                }

                if let Token::Text(text) = token {
                    state = State::AttrExpectingEqOrNamespace(text, span.start);
                }
            }
            State::StartTagExpectingGt => {
                if matches!(token, Token::Punc(Punc::TagEnd)) {
                    let name = path.pop().unwrap();

                    visitor.end_tag(name)?;

                    state = State::None;
                    continue;
                } else {
                    return Err(Error {
                        span,
                        kind: ErrorKind::SelfClosingTagNotClosed,
                    });
                }
            }
            State::EndTagExpectingGt(name) => {
                if matches!(token, Token::Punc(Punc::TagEnd)) {
                    visitor.end_tag(name)?;

                    state = State::None;
                    continue;
                } else {
                    return Err(Error {
                        span,
                        kind: ErrorKind::UnexpectedToken,
                    });
                }
            }
            State::EndTagExpectingName => {
                if matches!(token, Token::WhiteSpace(_)) {
                    continue;
                }

                if let Token::Text(name) = token {
                    if let Some(tag) = path.pop() {
                        if tag != name {
                            return Err(Error {
                                span,
                                kind: ErrorKind::MismatchedTag,
                            });
                        }

                        // debug!("End tag: {}", name);
                        state = State::EndTagExpectingGt(name);
                    } else {
                        return Err(Error {
                            span,
                            kind: ErrorKind::InvalidClosingTag,
                        });
                    }
                }
            }
            State::AttrExpectingEqOrNamespace(key, key_start) => {
                if matches!(token, Token::Punc(Punc::Equals)) {
                    state = State::AttrExpectingOpeningQuote(key);
                } else if matches!(token, Token::Punc(Punc::Colon)) {
                    state = State::AttrExpectingNamespaceLocalName(key, key_start);
                } else {
                    return Err(Error {
                        span,
                        kind: ErrorKind::InvalidAttributeSyntax,
                    });
                }
            }
            State::AttrExpectingNamespaceLocalName(_prefix, prefix_start) => {
                if let Token::Text(_local_name) = token {
                    let full_name = &input[prefix_start.index..span.end.index];
                    state = State::AttrExpectingEqOrNamespace(full_name, prefix_start);
                } else {
                    return Err(Error {
                        span,
                        kind: ErrorKind::InvalidAttributeSyntax,
                    });
                }
            }
            State::AttrExpectingValue(key, start_pos) => {
                if matches!(token, Token::Punc(Punc::QuoteMark)) {
                    let value = &input[start_pos.index..span.start.index];
                    visitor.attribute(key, value)?;
                    state = State::StartTagExpectingAttrs;
                } else {
                    state = State::AttrExpectingClosingQuote(key, start_pos);
                }
            }
            State::AttrExpectingOpeningQuote(key) => {
                if matches!(token, Token::Punc(Punc::QuoteMark)) {
                    state = State::AttrExpectingValue(key, span.end);
                } else {
                    return Err(Error {
                        span,
                        kind: ErrorKind::InvalidAttributeSyntax,
                    });
                }
            }
            State::AttrExpectingClosingQuote(key, start_pos) => {
                if matches!(token, Token::Punc(Punc::QuoteMark)) {
                    let value = &input[start_pos.index..span.start.index];
                    visitor.attribute(key, value)?;
                    state = State::StartTagExpectingAttrs;
                }
            }
            State::EntityRefExpectingValue(start_pos) => {
                if matches!(token, Token::Punc(Punc::NumberSign)) {
                    state = State::EntityRefExpectingNumerals(start_pos);
                } else if matches!(token, Token::Punc(Punc::Semicolon)) {
                    let entity_name = &input[start_pos.index..span.start.index];
                    visitor.entity_ref(entity_name)?;
                    state = State::None;
                } else if matches!(token, Token::Eof) {
                    return Err(Error {
                        span,
                        kind: ErrorKind::UnexpectedEof,
                    });
                } else {
                    state = State::EntityRefExpectingSemicolon(start_pos);
                }
            }
            State::EntityRefExpectingNumerals(start_pos) => {
                if matches!(token, Token::Punc(Punc::Semicolon)) {
                    let entity_value = &input[start_pos.index..span.start.index];
                    visitor.entity_ref(entity_value)?;
                    state = State::None;
                } else if matches!(token, Token::Eof) {
                    return Err(Error {
                        span,
                        kind: ErrorKind::UnexpectedEof,
                    });
                } else {
                    state = State::EntityRefExpectingSemicolon(start_pos);
                }
            }
            State::EntityRefExpectingSemicolon(start_pos) => {
                if matches!(token, Token::Punc(Punc::Semicolon)) {
                    let entity_name = &input[start_pos.index..span.start.index];
                    visitor.entity_ref(entity_name)?;
                    state = State::None;
                }
            }
            State::EntityDeclExpectingContent => {
                if matches!(token, Token::Punc(Punc::TagEnd)) {
                    state = State::None;
                    visitor.end_entity_declaration()?;
                    continue;
                }

                if let Some(value) = token.to_text() {
                    visitor.entity_declaration(value)?;
                    continue;
                }
            }
            State::ElementDeclExpectingContent => {
                if matches!(token, Token::WhiteSpace(_)) {
                    continue;
                }

                if matches!(token, Token::Punc(Punc::TagEnd)) {
                    state = State::None;
                    visitor.end_element_declaration()?;
                    continue;
                }

                if let Some(value) = token.to_text() {
                    visitor.element_declaration(value)?;
                    continue;
                }
            }
        }
    }

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
        debug!("{:?}", x.into_iter().collect::<Vec<_>>());

        stream_xml_events::<Vec<&'_ str>>(xemel, &mut XmlVisitor).unwrap();
        debug!();
        stream_xml_events::<Vec<&'_ str>>("&#00a0;", &mut XmlVisitor).unwrap();
    }

    #[test]
    fn test_entities() {
        // Test named entities
        stream_xml_events::<Vec<&'_ str>>("<root>&amp;&lt;&gt;&quot;</root>", &mut XmlVisitor)
            .unwrap();

        // Test numeric entities (decimal)
        stream_xml_events::<Vec<&'_ str>>("<root>&#65;&#66;&#67;</root>", &mut XmlVisitor).unwrap();

        // Test numeric entities (hexadecimal)
        stream_xml_events::<Vec<&'_ str>>("<root>&#x41;&#x42;&#x43;</root>", &mut XmlVisitor)
            .unwrap();

        // Test mixed entities and text
        stream_xml_events::<Vec<&'_ str>>(
            "<root>Hello &amp; goodbye &lt;world&gt;</root>",
            &mut XmlVisitor,
        )
        .unwrap();

        // Test multiple consecutive entities
        stream_xml_events::<Vec<&'_ str>>("<root>&amp;&amp;&amp;</root>", &mut XmlVisitor).unwrap();

        // Test entity between tags (no content)
        stream_xml_events::<Vec<&'_ str>>("<a></a>&amp;<b></b>", &mut XmlVisitor).unwrap();

        // Test entities with whitespace
        stream_xml_events::<Vec<&'_ str>>("<root> &amp; &lt; </root>", &mut XmlVisitor).unwrap();
    }

    #[test]
    fn test_processing_instructions() {
        // Test XML declaration
        stream_xml_events::<Vec<&'_ str>>(
            r#"<?xml version="1.0" encoding="UTF-8"?><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();

        // Test simple PI
        stream_xml_events::<Vec<&'_ str>>("<?target some data?><root/>", &mut XmlVisitor).unwrap();

        // Test PI with no data
        stream_xml_events::<Vec<&'_ str>>("<?target?><root/>", &mut XmlVisitor).unwrap();

        // Test PI after root element
        stream_xml_events::<Vec<&'_ str>>("<root/><?target data?>", &mut XmlVisitor).unwrap();

        // Test multiple PIs
        stream_xml_events::<Vec<&'_ str>>(
            "<?xml version=\"1.0\"?><?stylesheet href=\"style.css\"?><root/>",
            &mut XmlVisitor,
        )
        .unwrap();
    }

    #[test]
    fn test_doctype() {
        // Test simple DOCTYPE
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE html SYSTEM "blah.dtd" [ <!ENTITY foo "bar"> ] ><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();
    }

    #[test]
    fn test_various_doctype_formats() {
        // Test DOCTYPE with SYSTEM
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE root SYSTEM "foo.dtd"><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();

        // Test DOCTYPE with PUBLIC
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd"><html/>"#,
            &mut XmlVisitor,
        )
        .unwrap();

        // Test DOCTYPE before XML declaration (invalid but should parse)
        stream_xml_events::<Vec<&'_ str>>("<!DOCTYPE root><root/>", &mut XmlVisitor).unwrap();

        // Test DOCTYPE with internal subset
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE root [<!ENTITY foo "bar">]><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();

        // Test DOCTYPE with SYSTEM and internal subset (user's example)
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE TESTSUITE SYSTEM "testcases.dtd" [
                <!ENTITY % all "INCLUDE">
                <!ENTITY % dbnotn SYSTEM "dbnotn.mod">
                <!ENTITY % dbcent SYSTEM "dbcent.mod">
                <!ENTITY % dbpool SYSTEM "dbpool.mod">
                <!ENTITY % dbhier SYSTEM "dbhier.mod">
                <!ENTITY % dbgenent SYSTEM "dbgenent.mod">
            ]><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();

        // Test DOCTYPE with nested brackets (depth 2)
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE root [<!ELEMENT test [nested]>]><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();

        // Test DOCTYPE with multiple levels of nesting (depth 3)
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE root [<!ELEMENT test [level2 [level3]]>]><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();
    }

    #[test]
    fn test_depth_limit_doctype() {
        // Test that exceeding MAX_DOCTYPE_DEPTH (6) returns an error
        let result = stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE root [[[[[[[deep]]]]]]]><root/>"#,
            &mut XmlVisitor,
        );

        assert!(result.is_err());
        if let Err(err) = result {
            assert!(matches!(err.kind, ErrorKind::DoctypeNestingTooDeep));
        }
    }

    #[test]
    fn test_mixed_features() {
        // Test DOCTYPE + PI + elements + entities
        stream_xml_events::<Vec<&'_ str>>(
            r#"<?xml version="1.0"?><!DOCTYPE root><root attr="val">&amp;<child/></root>"#,
            &mut XmlVisitor,
        )
        .unwrap();
    }

    #[test]
    fn test_entity_declaration() {
        // Test simple entity declaration
        stream_xml_events::<Vec<&'_ str>>(r#"<!ENTITY foo "bar"><root/>"#, &mut XmlVisitor)
            .unwrap();

        // Test entity declaration with single quotes
        stream_xml_events::<Vec<&'_ str>>(r#"<!ENTITY nbsp '&#160;'><root/>"#, &mut XmlVisitor)
            .unwrap();

        // Test multiple entity declarations
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!ENTITY foo "bar"><!ENTITY baz "qux"><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();
    }

    #[test]
    fn test_entity_declaration_in_doctype() {
        // Test entity declaration inside DOCTYPE internal subset
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE root [<!ENTITY foo "bar">]><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();

        // Test multiple entity declarations in DOCTYPE
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE root [
                <!ENTITY foo "bar">
                <!ENTITY nbsp "&#160;">
                <!ENTITY copy "&#169;">
            ]><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();

        // Test DOCTYPE with SYSTEM and entity declarations
        stream_xml_events::<Vec<&'_ str>>(
            r#"<!DOCTYPE TESTSUITE SYSTEM "testcases.dtd" [
                <!ENTITY % all "INCLUDE">
                <!ENTITY % dbnotn "dbnotn.mod">
            ]><root/>"#,
            &mut XmlVisitor,
        )
        .unwrap();
    }
}
