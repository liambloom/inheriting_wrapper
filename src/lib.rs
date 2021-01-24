use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use std::{iter, ptr};
// I'm importing like 20 things (and counting) from syn, so I just used the *
use syn::{*, FnArg::*, ext::IdentExt, parse::{Parse, ParseStream}, punctuated::Punctuated};
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

enum Item {
    AbstractConst(AbstractConst),
    AbstractMethod(AbstractMethod),
    AbstractType(AbstractType),
    ConcreteConst(ImplItemConst),
    ConcreteMethod(ImplItemMethod),
    ConcreteType(ImplItemType),
    Verbatim(TokenStream2)
}

impl Parse for Item {
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
            let mut cursor = begin.cursor();
            let mut tokens = TokenStream2::new();
            loop {
                if let Some((tt, next)) = cursor.token_tree() {
                    tokens.extend(iter::once(tt));
                    cursor = next;
                }
                else {
                    break;
                }
            }
            Ok(Self::Verbatim(tokens))
        }
    }
}

#[proc_macro_attribute]
pub fn use_inner(args: TokenStream, item: TokenStream) -> TokenStream {
    use TraitItem::*;
  
    // parse_macro_input! calls parse(), but then handles the error properly
    let args = parse_macro_input!(args as UseInnerArg);// parse(args).unwrap();
    let item = parse_macro_input!(item as TraitItem); //parse::<TraitItem>(item)
    let out = match item {
        Const(item) => match item {
            TraitItemConst {attrs, const_token, ident, colon_token, ty, default: None, semi_token} => {
                let inner_type = args.get_type();

                let c = quote! {
                    #(#attrs)*
                    #const_token #ident #colon_token #ty = #inner_type::#ident #semi_token
                };

                c
            }
            _ => panic!("Const already has a value")
        },
        Type(item) => match item {
            TraitItemType { attrs, type_token, ident, generics, colon_token: None, 
                bounds, default: None, semi_token } => {
                    let inner_type = args.get_type();

                    if !bounds.is_empty() {
                        panic!("Unexpected type bound");
                    }

                    quote! {
                        #(#attrs)*
                        #type_token #ident #generics = #inner_type::ident #semi_token
                    }
                }
            _ => panic!("Type already has a value") // TODO could be unexpected token ":"
        } 
        Method(item) => match item {
            TraitItemMethod {attrs, sig, default: None, semi_token: Some(_)} => {
                let fn_name = &sig.ident;
                let mut fn_args = Punctuated::new();

                for arg in sig.inputs.pairs() {
                    let (arg, punct) = arg.into_tuple();
                    fn_args.push_value(match arg {
                        Typed(arg) => match *arg.pat {
                            Pat::Ident(ref arg) => &arg.ident,
                            _ => panic!("Expected identifier")
                        }
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
                            #sig {
                                self.#inner_field.#fn_name(#fn_args)
                            }
                        }
                    },
                    _ => { // static method
                        let inner_type = args.get_type();

                        quote! {
                            #(#attrs)*
                            #sig {
                                #inner_type::#fn_name(#fn_args)
                            }
                        }
                    }
                }
            },
            _ => panic!("Fn already has a body") // TODO: could also be "expected semicolon"
        }
        _ => panic!("Unsupported item type")
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
        if input.peek2(Token![:]) {
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
        if let PathArguments::AngleBracketed(generic) = &mut path.path.segments.last_mut().unwrap().arguments {
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