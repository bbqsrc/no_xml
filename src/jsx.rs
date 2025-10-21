use core::marker::PhantomData;

use collections2::ListMut;

use crate::{NonStandardEventHandler, NonStandardToken, Punc, Span, State, Token, Visitor};

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
pub struct JsxHandler<'s> {
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

impl Visitor for JsxVisitor {
    fn start_tag(&mut self, name: &str) {
        println!("Start: {}", name);
    }

    fn self_closed_tag(&mut self, name: &str) {
        println!("Self-closed: {name}");
    }

    fn end_tag(&mut self, name: &str) {
        println!("End: {name}");
    }

    fn attribute(&mut self, key: &str, value: &str) {
        println!("Attr: {key} => {value:?}")
    }

    fn text(&mut self, text: &str) {
        println!("Text: {:?}", text);
    }

    fn comment(&mut self, text: &str) {
        println!("Comment: {:?}", text);
    }

    fn entity(&mut self, entity: &str) {
        println!("Entity: {:?}", entity);
    }
}

impl<'s> NonStandardEventHandler<'s> for JsxHandler<'s> {
    type Token = JsxToken;
    type State = JsxState<'s>;
    type Visitor = JsxVisitor;

    fn process_state(
        &mut self,
        span: Span,
        token: Token<'s, Self::Token>,
        state: State<'s, Self::State>,
        path: &mut impl ListMut<&'s str>,
        visitor: &mut Self::Visitor,
        raw_input: &'s str,
    ) -> State<'s, Self::State> {
        // This is a skip state.
        if matches!(state, State::NonStandard(JsxState::None)) {
            return State::None;
        }

        if matches!(state, State::StartTagExpectingName) {
            if matches!(token, Token::Punc(Punc::TagEnd)) {
                visitor.start_fragment();
                path.push("#fragment");
                return JsxState::None.into();
            } else {
                return State::StartTagExpectingName;
            }
        }

        if matches!(state, State::EndTagExpectingName) {
            if matches!(token, Token::Punc(Punc::TagEnd)) {
                let leaf = path.pop().expect("Overpopped the path!");
                if leaf != "#fragment" {
                    panic!("Unmatched fragment, got: {:?}", leaf);
                }
                visitor.end_fragment();
                return JsxState::None.into();
            } else {
                return State::EndTagExpectingName;
            }
        }

        if let State::AttrExpectingOpeningQuote(key) = state {
            if matches!(token, Token::NonStandard(JsxToken::LeftParen)) {
                self.paren_count += 1;
                visitor.start_expr(span, key);
                return State::NonStandard(JsxState::ExpectingExpr(key));
            }
        }

        if let State::NonStandard(JsxState::ExpectingExpr(key)) = state {
            if matches!(token, Token::NonStandard(JsxToken::LeftParen)) {
                self.paren_count += 1;
            } else if matches!(token, Token::NonStandard(JsxToken::RightParen)) {
                self.paren_count -= 1;

                if self.paren_count == 0 {
                    visitor.end_expr(span, key, raw_input);
                    return State::StartTagExpectingAttrs;
                }
            }

            return state;
        }

        state
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
        println!("{:?}", x.into_iter().collect::<Result<Vec<_>, _>>());

        stream_events::<JsxHandler, Vec<&'_ str>>(xemel, &mut JsxVisitor::new()).unwrap();
        println!();
        stream_events::<JsxHandler, Vec<&'_ str>>("&#00a0;", &mut JsxVisitor::new()).unwrap();
    }
}
