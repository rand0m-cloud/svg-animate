use crate::parse::*;

macro_rules! tokens {
    ($(($ty: ident, $token:literal)),+ $(,)*) => {
        $(tokens!($ty, $token);)+
    };
    ($ty: ident, $token: literal) => {
        #[derive(Debug, Clone, Parse, PartialEq)]
        pub struct $ty(#[token($token)] pub Token);

        impl ToString for $ty {
            fn to_string(&self) -> String {
                $token.to_string()
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
    (Animate, "animate"),
    (Delay, "delay"),
    (Animation, "animation"),
    (Play, "play"),
    (Use, "use"),
    (Break, "break"),
    (Arg, "arg"),
    (SvgAssign, "svg_assign"),
    (SvgNew, "svg_new"),
    (SvgAppend, "svg_append"),
    (Call, "call"),
    (Intrinsic, "intrinsic"),
    (GetField, "getfield"),
);
