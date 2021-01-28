use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use std::{iter, ptr};
// I'm importing like 20 things (and counting) from syn, so I just used the *
use syn::{*, FnArg::*, ext::IdentExt, parse::{Parse, ParseStream, ParseBuffer}, punctuated::Punctuated};
use quote::quote;

// A lot of this code is copied (and modified) from syn
// TODO: Add credit (not legally required, syn can be licensed under apache-2.0 and has no NOTICE file)

#[derive(Clone)]
struct AbstractConst {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub defaultness: Option<Token![default]>,
    pub const_token: Token![const],
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Type,
    pub semi_token: Token![;],
}

impl Parse for AbstractConst {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(AbstractConst {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            defaultness: input.parse()?,
            const_token: input.parse()?,
            ident: {
                let lookahead = input.lookahead1();
                if lookahead.peek(Ident) || lookahead.peek(Token![_]) {
                    input.call(Ident::parse_any)?
                } else {
                    return Err(lookahead.error());
                }
            },
            colon_token: input.parse()?,
            ty: input.parse()?,
            semi_token: input.parse()?,
        })
    }
}

#[derive(Clone)]
struct AbstractMethod {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub defaultness: Option<Token![default]>,
    pub sig: Signature,
    pub semi_token: Token![;],
}

impl Parse for AbstractMethod {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(AbstractMethod {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            defaultness: input.parse()?,
            sig: input.parse()?,
            semi_token: input.parse()?,
        })
    }
}

#[derive(Clone)]
struct AbstractType {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub defaultness: Option<Token![default]>,
    pub type_token: Type,
    pub ident: Ident,
    pub generics: Generics,
    pub semi_token: Token![;],
}

impl Parse for AbstractType {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(AbstractType {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            defaultness: input.parse()?,
            type_token: input.parse()?,
            ident: input.parse()?,
            generics: {
                let mut generics: Generics = input.parse()?;
                generics.where_clause = input.parse()?;
                generics
            },
            semi_token: input.parse()?,
        })
    }
}

enum AbstractItem {
    Const(AbstractConst),
    Method(AbstractMethod),
    Type(AbstractType),
}

impl Parse for AbstractItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let ahead = input.fork();
        let vis = input.parse()?;
        
        let mut lookahead = input.lookahead1();
        let defaultness = if lookahead.peek(Token![default]) && !input.peek2(Token![!]) {
            let defaultness: Token![default] = input.parse()?;
            lookahead = ahead.lookahead1();
            Some(defaultness)
        } else {
            None
        };

        if input.peek(Token![const]) {
            Ok(Self::Const(AbstractConst {
                attrs,
                vis,
                defaultness,
                const_token: input.parse()?,
                ident: {
                    let lookahead = input.lookahead1();
                    if lookahead.peek(Ident) || lookahead.peek(Token![_]) {
                        input.call(Ident::parse_any)?
                    } else {
                        return Err(lookahead.error());
                    }
                },
                colon_token: input.parse()?,
                ty: input.parse()?,
                semi_token: input.parse()?,
            }))
        }
        else if input.peek(Token![type]) {
            Ok(Self::Type(AbstractType {
                attrs,
                vis,
                defaultness,
                type_token: input.parse()?,
                ident: input.parse()?,
                generics: {
                    let mut generics: Generics = input.parse()?;
                    generics.where_clause = input.parse()?;
                    generics
                },
                semi_token: input.parse()?,
            }))
        }
        else if input.peek(Token![fn]) {
            Ok(Self::Method(AbstractMethod {
                attrs,
                vis,
                defaultness,
                sig: input.parse()?,
                semi_token: input.parse()?,
            }))
        }
        else {
            Err(input.error("Expected `const', `type', or `fn'"))
        }
    }
}

enum MaybeAbstractItem {
    AbstractConst(AbstractConst),
    AbstractMethod(AbstractMethod),
    AbstractType(AbstractType),
    ConcreteConst(ImplItemConst),
    ConcreteMethod(ImplItemMethod),
    ConcreteType(ImplItemType),
    Verbatim(TokenStream2)
}

impl Parse for MaybeAbstractItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let begin = input.fork();
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse()?;
        
        let mut lookahead = input.lookahead1();
        let defaultness = if lookahead.peek(Token![default]) && !input.peek2(Token![!]) {
            let defaultness: Token![default] = input.parse()?;
            lookahead = input.lookahead1();
            Some(defaultness)
        } else {
            None
        };

        if input.peek(Token![const]) {
            let const_token = input.parse()?;
            let ident = {
                let lookahead = input.lookahead1();
                if lookahead.peek(Ident) || lookahead.peek(Token![_]) {
                    input.call(Ident::parse_any)?
                } else {
                    return Err(lookahead.error());
                }
            };
            let colon_token = input.parse()?;
            let ty = input.parse()?;
            if input.peek(Token![=]) {
                Ok(Self::ConcreteConst(ImplItemConst {
                    attrs,
                    vis,
                    defaultness,
                    const_token,
                    ident,
                    colon_token,
                    ty,
                    eq_token: input.parse()?,
                    expr: input.parse()?,
                    semi_token: input.parse()?,
                }))
            }
            else {
                Ok(Self::AbstractConst(AbstractConst {
                    attrs,
                    vis,
                    defaultness,
                    const_token,
                    ident,
                    colon_token,
                    ty,
                    semi_token: input.parse()?,
                }))
            }
        }
        else if input.peek(Token![type]) {
            todo!()
        }
        else if input.peek(Token![fn]) {
            todo!()
        }
        else {
            Ok(Self::Verbatim(verbatim_between(begin, input)))
        }
    }
}

// Taken from syn
fn verbatim_between<'a>(begin: ParseBuffer<'a>, end: ParseStream<'a>) -> TokenStream2 {
    let end = end.cursor();
    let mut cursor = begin.cursor();
    let mut tokens = TokenStream2::new();
    while cursor != end {
        let (tt, next) = cursor.token_tree().unwrap();
        tokens.extend(iter::once(tt));
        cursor = next;
    }
    tokens
}

#[proc_macro_attribute]
pub fn use_inner(args: TokenStream, item: TokenStream) -> TokenStream {
    use AbstractItem::*;
  
    // parse_macro_input! calls parse(), but then handles the error properly
    let args = parse_macro_input!(args as UseInnerArg);
    let item = parse_macro_input!(item as AbstractItem);

    let out = match item {
        Const(item) => match item {
            AbstractConst {attrs, vis, defaultness, const_token, ident, colon_token, ty, semi_token} => {
                let inner_type = args.get_type();

                let c = quote! {
                    #(#attrs)*
                    #vis #defaultness #const_token #ident #colon_token #ty = #inner_type::#ident #semi_token
                };

                c
            }
        },
        Type(item) => match item {
            AbstractType { attrs, vis, defaultness, type_token, ident, generics, semi_token } => {
                let inner_type = args.get_type();

                quote! {
                    #(#attrs)*
                    #vis #defaultness #type_token #ident #generics = #inner_type::ident #semi_token
                }
            }
        } 
        Method(item) => match item {
            AbstractMethod {attrs, vis, defaultness, sig, .. } => {
                let fn_name = &sig.ident;
                let mut fn_args = Punctuated::new();

                for arg in sig.inputs.pairs() {
                    let (arg, punct) = arg.into_tuple();
                    fn_args.push_value(match arg {
                        Typed(arg) => arg,
                        Receiver(_) => continue,
                    });
                    if let Some(punct) = punct {
                        fn_args.push_punct(punct);
                    }
                }

                match sig.inputs.first() {
                    Some(Receiver(_)) => { // instance method
                        let inner_field = args.get_ident();

                        quote! {
                            #(#attrs)*
                            #vis #defaultness #sig {
                                self.#inner_field.#fn_name(#fn_args)
                            }
                        }
                    },
                    _ => { // static method
                        let inner_type = args.get_type();

                        quote! {
                            #(#attrs)*
                            #vis #defaultness #sig {
                                #inner_type::#fn_name(#fn_args)
                            }
                        }
                    }
                }
            }
        }
    };
    out.into()
}

enum UseInnerArg {
    Full { 
        field: Ident,
        ty: Type
    },
    Either {
        field: *const Ident,
        ty: Type,
    },
}

impl Parse for UseInnerArg {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek2(Token![:]) && !input.peek2(Token![::]) {
            Ok(Self::Full {
                field: input.parse()?,
                ty: {
                    input.parse::<Token![:]>()?;
                    let mut ty = input.parse()?;
                    add_fishtail(&mut ty);
                    ty
                },
            })
        }
        else {
            let mut ty = input.parse()?;
            add_fishtail(&mut ty);
            Ok(Self::Either {
                field: {
                    // TODO: Maybe make lazy?
                    // Probably not, despite being a fairly large block of code, it's not that computationally intensive
                    // And making it lazy would require either mutability or multiple pointers/references
                    if let Type::Path(TypePath { qself: None, path: Path { leading_colon: None, segments} }) = &ty {
                        if segments.len() == 1 {
                            if let PathArguments::None = segments[0].arguments {
                                &segments[0].ident as *const Ident
                            }
                            else {
                                ptr::null()
                            }
                        }
                        else {
                            ptr::null()
                        }
                    }
                    else {
                        ptr::null()
                    }
                },
                ty,
            })
        }
    }
}

fn add_fishtail(ty: &mut Type) {
    if let Type::Path(path) = ty {
        if let PathArguments::AngleBracketed(generic) = &mut path.path.segments.last_mut().expect("Empty path").arguments {
            if generic.colon2_token.is_none() {
                generic.colon2_token = Some(parse(quote! { :: }.into()).unwrap());
            }
        }
    }
}

impl UseInnerArg {
    pub fn get_ident<'a>(&'a self) -> Option<&'a Ident> {
        match self {
            Self::Full { field, .. } => Some(field),
            Self::Either { field, .. } => unsafe { field.as_ref() },
        }
    }
    
    pub fn get_type(&self) -> &Type {
        match self {
            Self::Full { ty, .. } => ty,
            Self::Either { ty, .. } => ty
        }
    }
}