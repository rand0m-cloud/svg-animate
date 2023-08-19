use proc_macro::{self, TokenStream};
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Data, DataEnum, DataStruct, DeriveInput, Fields,
    Generics, Lifetime, LitStr, Token,
};

/// pub trait Parse<'a>: Sized {
///     fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
///     where
///         'a: 'b;
/// }
///
/// pub trait ParseOwned: Sized {
///     fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
///     where
///         'a: 'b;
///
///     fn parse_from_str(input: &str) -> Option<Self> {
///         let tokens = lex(input);
///         Some(Self::parse_owned(&tokens)?.1)
///     }
/// }

#[proc_macro_derive(Parse, attributes(token))]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse_macro_input!(input);
    let output = match data {
        Data::Struct(st) => do_struct(&ident, generics, &st, true),
        Data::Enum(en) => do_enum(&ident, generics, &en, true),
        Data::Union(_) => panic!("no unions"),
    };
    output.into()
}

#[proc_macro_derive(ParseOwned, attributes(token))]
pub fn derive_owned(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse_macro_input!(input);
    let output = match data {
        Data::Struct(st) => do_struct(&ident, generics, &st, false),
        Data::Enum(en) => do_enum(&ident, generics, &en, false),
        Data::Union(_) => panic!("no unions"),
    };
    output.into()
}

fn generate_statement(field: &syn::Field, index: usize, owned: bool) -> TokenStream2 {
    let ident = &field
        .ident
        .as_ref()
        .cloned()
        .unwrap_or_else(|| Ident::new(&format!("_{}", index), Span::mixed_site()));

    let token = field.attrs.iter().find_map(|attr| {
        attr.path()
            .get_ident()
            .and_then(|i| (i == "token").then_some(()))?;
        attr.parse_args::<LitStr>().ok()
    });
    match (token, owned) {
        (None, false) => {
            quote! { let (input, #ident) = Parse::parse(input)?; }
        }
        (Some(t), false) => {
            quote! {
                let (input, #ident) = Token::parse_token(#t, input)?;
            }
        }
        (None, true) => {
            quote! { let (input, #ident) = ParseOwned::parse_owned(input)?; }
        }
        (Some(t), true) => {
            quote! {
                let (input, #ident) = Token::parse_token_owned(#t, input)?;
            }
        }
    }
}

fn do_fields(fields: &Fields, tail_prefix: TokenStream2, owned: bool) -> TokenStream2 {
    let statements = fields
        .into_iter()
        .enumerate()
        .flat_map(|(i, f)| generate_statement(f, i, owned))
        .collect::<TokenStream2>();
    let tail = match fields {
        Fields::Named(named) => {
            let fields = named
                .named
                .iter()
                .map(|f| f.ident.as_ref().cloned().unwrap())
                .collect::<Punctuated<Ident, Token![,]>>();
            quote! {{#fields}}
        }
        Fields::Unnamed(unnamed) => {
            let fields = unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| Ident::new(&format!("_{i}"), Span::mixed_site()))
                .collect::<Punctuated<Ident, Token![,]>>();
            quote! {(#fields)}
        }
        Fields::Unit => panic!("unit struct"),
    };

    quote! {
        (||{
            #statements
            Some((input, #tail_prefix #tail))
        })()
    }
}

fn do_struct(
    ident: &Ident,
    mut generics: Generics,
    st: &DataStruct,
    generate_ref_impl: bool,
) -> TokenStream2 {
    let ref_impl = if generate_ref_impl {
        let parse_exprs = do_fields(&st.fields, quote! {Self}, false);
        quote! {
            impl<'a> Parse<'a> for #ident #generics {
                fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)> where 'a:'b{
                    #parse_exprs
                }
            }
        }
    } else {
        quote! {}
    };

    let parse_owned_exprs = do_fields(&st.fields, quote! {Self}, true);

    if let Some(lifetime) = generics.lifetimes_mut().next() {
        lifetime.lifetime = Lifetime::new("'static", Span::mixed_site());
    };

    quote! {
        #ref_impl

        impl ParseOwned  for #ident #generics {
            fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
            where
                'a: 'b {
                #parse_owned_exprs
            }
        }
    }
}

fn do_enum(
    ident: &Ident,
    mut generics: Generics,
    en: &DataEnum,
    generate_ref_impl: bool,
) -> TokenStream2 {
    let make_variants = |owned| {
        en.variants
            .iter()
            .flat_map(|v| {
                let variant = &v.ident;
                let parse_exprs = do_fields(&v.fields, quote! {Self:: #variant}, owned);
                quote! {
                    if let v@Some(_) = { #parse_exprs } {
                        return v;
                    }
                }
            })
            .collect::<TokenStream2>()
    };

    let ref_impl = if generate_ref_impl {
        let variants = make_variants(false);
        quote! {
            impl<'a> Parse<'a> for #ident #generics{
                fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)> where 'a:'b{
                    #variants
                    None
                }
            }
        }
    } else {
        quote! {}
    };

    let owned_variants = make_variants(true);

    if let Some(lifetime) = generics.lifetimes_mut().next() {
        lifetime.lifetime = Lifetime::new("'static", Span::mixed_site());
    };

    quote! {
        #ref_impl

        impl ParseOwned  for #ident #generics {
            fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
            where
                'a: 'b {
                #owned_variants
                None
            }
        }
    }
}
