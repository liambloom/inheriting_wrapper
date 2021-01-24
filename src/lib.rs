use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use std::iter;
use syn::{self, Attribute, FnArg::{self, *}, Generics, Ident, ImplItemConst, ImplItemMethod, ImplItemType, Pat, PathArguments, Result, Signature, Token, TraitItem, TraitItemConst, TraitItemMethod, TraitItemType, Type, Visibility, ext::IdentExt, parse::{Parse, ParseStream}, parse, punctuated::Punctuated};
use quote::{quote};

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
    Verbatim(TokenStream)
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
            Ok(Self::Verbatim(tokens.into()))
        }
    }
}

#[proc_macro_attribute]
pub fn use_inner(args: TokenStream, item: TokenStream) -> TokenStream {
    if let Ok(item) = parse::<TraitItem>(item) {
        use TraitItem::*;
        let out = match item {
            Const(item) => match item {
                TraitItemConst {attrs, const_token, ident, colon_token, ty, default: None, semi_token} => {
                    let inner_type = get_type(args);

                    let c = quote! {
                        #(#attrs)*
                        #const_token #ident #colon_token #ty = #inner_type::#ident #semi_token
                    };

                    println!("What?");
                    println!("{}", c.clone());

                    c
                }
                _ => panic!("Const already has a value")
            },
            Type(item) => match item {
                TraitItemType { attrs, type_token, ident, generics, colon_token: None, 
                    bounds, default: None, semi_token } => {
                        let inner_type = get_type(args);

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
                            Typed(arg) => match *arg.pat.clone() {
                                Pat::Ident(arg) => arg.ident,
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
                            let inner_field = get_ident(args);

                            quote! {
                                #(#attrs)*
                                #sig {
                                    self.#inner_field.#fn_name(#fn_args)
                                }
                            }
                        },
                        _ => { // static method
                            let inner_type = get_type(args);

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
    /*else if let Ok(item) = parse::<ItemImpl>(item) {
        todo!()
    }*/
    // maybe an else-if-let
    else {
        panic!("Not an abstract item")
    }
    
    // let arg = parse::<PatType? maybe>::(args)
    // parse::<Anything that implements UseInner>::(item).use_inner(arg.ident, arg.type)
}

fn get_ident(args: TokenStream) -> Ident {
    match parse_args(args.clone()) {
        Some((ident, _)) => ident,
        None => parse(args).unwrap()
    }
}

fn get_type(args: TokenStream) -> Type {
    // TODO add fishtail
    let mut ty = match parse_args(args.clone()) {
        Some((_, ty)) => ty,
        None => parse(args).unwrap()
    };
    if let Type::Path(path) = &mut ty {
        if let PathArguments::AngleBracketed(generic) = &mut path.path.segments.last_mut().unwrap().arguments {
            /*match generic {
                AngleBracketedGenericArguments { colon2_token: None, lt_token, args, gt_token } => {
                    println!("adding fishtail");
                    *generic = parse(quote! { ::#lt_token #args #gt_token }.into()).unwrap()
                }
                _ => ()
            }*/
            if generic.colon2_token.is_none() {
                //println!("adding fishtail");
                generic.colon2_token = Some(parse(quote! { :: }.into()).unwrap());
                //assert!(generic.colon2_token.is_some());
            }
        }
    }
    let ty_clone = ty.clone();
    println!("{}", quote! { #ty_clone });
    ty
}

fn parse_args(args: TokenStream) -> Option<(Ident, Type)> {
    match parse::<FnArg>(args.clone()) {
        Ok(inner) => {
            match inner {
                Typed(inner) => {
                    if inner.attrs.len() != 0 {
                        panic!("Unexpected attribute")
                    }
                    else {
                        match *inner.pat {
                            Pat::Ident(inner_ident) => {
                                if inner_ident.attrs.len() != 0 || inner_ident.by_ref.is_some() 
                                    || inner_ident.mutability.is_some() || inner_ident.subpat.is_some() 
                                {
                                    panic!("Unexpected token")
                                }
                                else {
                                    Some((inner_ident.ident, *inner.ty))
                                }
                            }
                            _ => None
                        }
                    }
                }
                _ => panic!("`self' is not a valid argument")
            }
        }
        Err(_) => None
    }
}

// impl UseInner for impl block and anything that it can contain (abstract const, abstract fn, etc.)

/*#[proc_macro_attribute]
pub fn impl_wrapper(args: TokenStream, item: TokenStream) -> TokenStream {
    /*let wrapper: GenericPath = syn::parse(args).unwrap();
    let ast: ItemImpl = syn::parse(item).unwrap();*/
    if let Ok(wrapper) = syn::parse::<GenericPath>(args) {   
        if let Ok(ast) = syn::parse::<ItemImpl>(item) {
            let result = quote! {
                #ast

                /*impl #arg {
                    #(#items)*
                }*/
            };
            result.into()
        }
        else {
            panic!()
        }
    }
    else {
        panic!()
    }
    // parse_macro_input!(item as ItemImpl);
    //let wrapper = parse_macro_input!(args as Constraint);
    //let args = parse_macro_input!(attr as AttributeArgs);'
    /*for arg in attr {
        println!("{}", arg)
    }*/

    /*let arg;
    if args.len() == 1 {
        if let NestedMeta::Meta(meta) = &args[0] {
            if let Meta::Path(path) = meta {
                arg = path;
            }
            else {
                panic!("Expected path, received something else")
            }
        }
        else {
            panic!("Expected meta-item, received literal")
        }
    }
    else {
        panic!("Expected 1 argument, found {}", args.len());
    }
    let name = &ast.self_ty;
    let items = &ast.items;*/

    
}*/