use core::fmt::Debug;
use core::marker::PhantomData;

use collections2::ListMut;

use crate::{Error, ErrorKind, NonStandardToken, Parser, Punc, Span, State, Token, Visitor};

#[derive(Debug, Clone, Copy)]
pub enum JsxState<'s> {
    None,
    ExpectingExpr(&'s str),
}

impl<'s> From<JsxState<'s>> for State<'s, JsxState<'s>> {
    fn from(state: JsxState<'s>) -> Self {
        State::NonStandard(state)
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct JsxParser<'s> {
    paren_count: usize,
    _data: PhantomData<&'s ()>,
}

#[derive(Debug, Default)]
pub struct JsxVisitor {
    cur_expr: Option<Span>,
}

impl JsxVisitor {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Visitor for JsxVisitor {
    fn open_tag(&mut self, name: &str) -> Result<(), Error> {
        println!("Start: {}", name);
        Ok(())
    }

    fn close_open_tag(&mut self, name: &str, self_closing: bool) -> Result<(), Error> {
        println!("Close open: {} (self_closing: {})", name, self_closing);
        Ok(())
    }

    fn start_close_tag(&mut self, name: &str) -> Result<(), Error> {
        println!("End: {name}");
        Ok(())
    }

    fn end_close_tag(&mut self, name: &str) -> Result<(), Error> {
        println!("End close: {name}");
        Ok(())
    }

    fn attribute(&mut self, key: &str, value: &str) -> Result<(), Error> {
        println!("Attr: {key} => {value:?}");
        Ok(())
    }

    fn text(&mut self, text: &str) -> Result<(), Error> {
        println!("Text: {:?}", text);
        Ok(())
    }

    fn comment(&mut self, text: &str) -> Result<(), Error> {
        println!("Comment: {:?}", text);
        Ok(())
    }

    fn start_comment(&mut self) -> Result<(), Error> {
        println!("Start comment");
        Ok(())
    }

    fn end_comment(&mut self) -> Result<(), Error> {
        println!("End comment");
        Ok(())
    }

    fn entity_ref(&mut self, entity: &str) -> Result<(), Error> {
        println!("Entity: {:?}", entity);
        Ok(())
    }
}

impl JsxVisitor {
    fn start_fragment(&mut self) {
        println!("Start fragment");
    }

    fn end_fragment(&mut self) {
        println!("End fragment");
    }

    fn start_expr(&mut self, span: Span, name: &str) {
        println!("Start expression with key: {span} {name:?}");
        self.cur_expr = Some(span);
    }

    fn end_expr(&mut self, span: Span, name: &str, raw_input: &str) {
        let start = self.cur_expr.take().unwrap().end;
        let end = span.start;
        let span = Span { start, end };

        println!(
            "End expression with key: {span} {name:?} => {:?}",
            &raw_input[span.as_range()]
        );
    }
}

impl Parser<JsxVisitor> for JsxParser<'_> {
    type Token = JsxToken;
    type State<'s> = JsxState<'s>;

    fn process_state<'s>(
        &mut self,
        span: Span,
        token: Token<'s, Self::Token>,
        state: State<'s, Self::State<'s>>,
        path: &mut impl ListMut<&'s str>,
        visitor: &mut JsxVisitor,
        raw_input: &'s str,
    ) -> Result<State<'s, Self::State<'s>>, Error> {
        // This is a skip state.
        if matches!(state, State::NonStandard(JsxState::None)) {
            return Ok(State::None);
        }

        if matches!(state, State::StartTagExpectingName) {
            if matches!(token, Token::Punc(Punc::TagEnd)) {
                visitor.start_fragment();
                path.push("#fragment");
                return Ok(JsxState::None.into());
            } else {
                return Ok(State::StartTagExpectingName);
            }
        }

        if matches!(state, State::EndTagExpectingName) {
            if matches!(token, Token::Punc(Punc::TagEnd)) {
                let leaf = path.pop().expect("Overpopped the path!");
                if leaf != "#fragment" {
                    return Err(Error {
                        span,
                        kind: ErrorKind::MismatchedTag,
                    });
                }
                visitor.end_fragment();
                return Ok(JsxState::None.into());
            } else {
                return Ok(State::EndTagExpectingName);
            }
        }

        if let State::AttrExpectingOpeningQuote(key) = state {
            if matches!(token, Token::NonStandard(JsxToken::LeftParen)) {
                self.paren_count += 1;
                visitor.start_expr(span, key);
                return Ok(State::NonStandard(JsxState::ExpectingExpr(key)));
            }
        }

        if let State::NonStandard(JsxState::ExpectingExpr(key)) = state {
            if matches!(token, Token::NonStandard(JsxToken::LeftParen)) {
                self.paren_count += 1;
            } else if matches!(token, Token::NonStandard(JsxToken::RightParen)) {
                self.paren_count -= 1;

                if self.paren_count == 0 {
                    visitor.end_expr(span, key, raw_input);
                    return Ok(State::StartTagExpectingAttrs);
                }
            }

            return Ok(state);
        }

        Ok(state)
    }
}

impl NonStandardToken for JsxToken {
    fn handle_char(ch: char) -> Option<Self> {
        Some(match ch {
            '{' => Self::LeftParen,
            '}' => Self::RightParen,

            _ => return None,
        })
    }
}

#[derive(Debug, Clone)]
pub enum JsxToken {
    LeftParen,
    RightParen,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Cursor, stream_events};

    #[test]
    fn test() {
        let mut x = Cursor::<'_, ()>::new("abcdefghijk");
        println!("{:?}", x.peek_str());
        x.buffer_char();
        println!("{:?}", x.peek_str());
        x.buffer_char();
        println!("{:?}", x.peek_str());
        x.consume();
        println!("{:?}", x.peek_str());
        x.buffer_char();
        x.buffer_char();
        x.buffer_char();
        x.buffer_char();
        println!("{:?}", x.peek_str());

        let xemel = "<><root>
        <a jsx-shit={{ \"potato\": true, another: `thing`}} />< a ></
        a> &#0a00;&amp;
        <b/><c  attr=\"lol\"><d>lol<!-- this is a comment
        still a comment
        another
        -->
        </d>dangler = | : / <![CDATA[this is &raw;]]>
    </c>
    </root></>AWOO";

        let x = Cursor::<'_, JsxToken>::new(xemel);
        println!("{:?}", x.into_iter().collect::<Vec<_>>());

        stream_events::<Vec<&'_ str>, JsxParser, _>(xemel, &mut JsxVisitor::new()).unwrap();
        println!();
        stream_events::<Vec<&'_ str>, JsxParser, _>("&#00a0;", &mut JsxVisitor::new()).unwrap();
    }
}
