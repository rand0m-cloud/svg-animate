use crate::parse::*;

macro_rules! tokens {
    ($(($ty: ident, $token:literal)),+ $(,)*) => {
        $(tokens!($ty, $token);)+
    };
    ($ty: ident, $token: literal) => {
        #[derive(Debug, Clone,  ParseOwned, PartialEq)]
        pub struct $ty(#[token($token)] Token<'static>);

        impl $ty {
            #[allow(dead_code)]
            pub fn new() -> Self {
                Self(Token::new($token))
            }

            #[allow(dead_code)]
            pub fn span(&self) -> OwnedSpan {
                self.0.span()
            }

            #[allow(dead_code)]
            pub fn set_span(&mut self, span: OwnedSpan)  {
                self.0.set_span(span)
            }
        }
    };
}

tokens!(
    (Semicolon, ";"),
    (Colon, ":"),
    (Ampersand, "&"),
    (Minus, "-"),
    (Plus, "+"),
    (Bang, "!"),
    (Comma, ","),
    (Dot, "."),
    (Equals, "="),
    (Pipe, "|"),
    (Star, "*"),
    (OpenBrace, "{"),
    (CloseBrace, "}"),
    (OpenParen, "("),
    (CloseParen, ")"),
    (OpenAngle, "<"),
    (CloseAngle, ">"),
    (OpenSquareBracket, "["),
    (CloseSquareBracket, "]"),
    (Slash, "/"),
    (Null, "null"),
    (Return, "return"),
    (S, "s"),
    (Fork, "fork"),
    (Animate, "animate")
);
