use proc_macro::{self, TokenStream};
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Data, DataEnum, DataStruct, DeriveInput, Fields,
    LitStr, Token,
};

/// pub trait Parse: Sized {
///     fn parse(input: &[Token]) -> Option<(&[Token], Self)>
/// }

#[proc_macro_derive(Parse, attributes(token))]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    let output = match data {
        Data::Struct(st) => do_struct(&ident, &st),
        Data::Enum(en) => do_enum(&ident, &en),
        Data::Union(_) => panic!("no unions"),
    };
    output.into()
}

fn generate_statement(field: &syn::Field, index: usize) -> TokenStream2 {
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
    match token {
        Some(t) => {
            quote! {
                let (input, #ident) = Token::parse_token(#t, input)?;
            }
        }
        None => {
            quote! { let (input, #ident) = Parse::parse(input)?; }
        }
    }
}

fn do_fields(fields: &Fields, tail_prefix: TokenStream2) -> TokenStream2 {
    let statements = fields
        .into_iter()
        .enumerate()
        .flat_map(|(i, f)| generate_statement(f, i))
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

fn do_struct(ident: &Ident, st: &DataStruct) -> TokenStream2 {
    let parse_exprs = do_fields(&st.fields, quote! {Self});
    quote! {
        impl Parse for #ident {
            fn parse(input: &[Token]) -> Option<(&[Token], Self)>{
                #parse_exprs
            }
        }
    }
}

fn do_enum(ident: &Ident, en: &DataEnum) -> TokenStream2 {
    let variants = en
        .variants
        .iter()
        .flat_map(|v| {
            let variant = &v.ident;
            let parse_exprs = do_fields(&v.fields, quote! {Self:: #variant});
            quote! {
                if let v@Some(_) = { #parse_exprs } {
                    return v;
                }
            }
        })
        .collect::<TokenStream2>();

    quote! {
        impl Parse for #ident {
            fn parse(input: &[Token]) -> Option<(&[Token], Self)>
                {
                #variants
                None
            }
        }
    }
}
