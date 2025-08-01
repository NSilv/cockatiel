use core::panic;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, punctuated::Punctuated, DeriveInput, Ident, Token};

#[proc_macro_derive(AnimationEventPayload)]
pub fn animation_event_derive(event: TokenStream) -> TokenStream {
  animation_event_derive_impl(event)
}

fn animation_event_derive_impl(event: TokenStream) -> TokenStream {
  let DeriveInput { ident, .. } = parse_macro_input!(event as DeriveInput);
  quote! {
      impl AnimationEventPayload for #ident {}
  }
  .into()
}

#[proc_macro_derive(AnimationShift)]
pub fn animation_shift_derive(event: TokenStream) -> TokenStream {
  animation_shift_derive_impl(event)
}

fn animation_shift_derive_impl(event: TokenStream) -> TokenStream {
  let DeriveInput { ident, .. } = parse_macro_input!(event as DeriveInput);
  quote! {
      impl AnimationShift for #ident {}
  }
  .into()
}
#[proc_macro_derive(AnimationInput)]
pub fn animation_input_derive(input: TokenStream) -> TokenStream {
  animation_input_derive_impl(input)
}
fn animation_input_derive_impl(input: TokenStream) -> TokenStream {
  let DeriveInput { ident, data, .. } = parse_macro_input!(input as DeriveInput);
  use syn::Data::*;
  let (vars_enum_ident, vars_enum, get_impl) = match data {
    Enum(_) | Union(_) => panic!("Enums and unions are not supported"),

    Struct(syn::DataStruct {
      fields: syn::Fields::Unnamed(_) | syn::Fields::Unit,
      ..
    }) => panic!("Structs with unnamed fields are not supported"),
    Struct(syn::DataStruct {
      fields: syn::Fields::Named(fields),
      ..
    }) => generate_vars_struct(fields),
  };
  quote! {
      #vars_enum
      impl AnimationInput for #ident {
          type Vars = #vars_enum_ident;

          fn get(&self, var: &Self::Vars) -> InputValue {
              match *var {
                  #(#get_impl),*
              }
          }

      }
  }
  .into()
}

fn generate_vars_struct(
  fields: syn::FieldsNamed,
) -> (
  syn::Ident,
  proc_macro2::TokenStream,
  Vec<proc_macro2::TokenStream>,
) {
  let vars: Vec<syn::Ident> = fields
    .named
    .iter()
    .map(|field| {
      let ident = field.ident.as_ref().expect("ident is none").to_string();
      let var_name = ident
        .split("_")
        .map(capitalize)
        .collect::<Vec<_>>()
        .join("");
      syn::Ident::new(&var_name, proc_macro2::Span::call_site())
    })
    .collect();
  let ident = syn::Ident::new("Vars", proc_macro2::Span::call_site());
  let mut get_impls: Vec<proc_macro2::TokenStream> = fields
    .named
    .iter()
    .zip(vars.iter())
    .map(|(field, var)| {
      let input_value = choose_inputvalue(field);
      let field = field.ident.as_ref().unwrap();
      quote! {
          #ident::#var => {
              InputValue::#input_value(self.#field)
          }
      }
    })
    .collect();
  get_impls.push(quote! {
      _ => {
          unreachable!()
      }
  });
  let enum_struct = quote! {
      #[derive(Clone, Debug)]
      pub enum #ident {
          #(#vars,)*
      }
  };

  (ident, enum_struct, get_impls)
}
fn choose_inputvalue(field: &syn::Field) -> syn::Ident {
  match &field.ty {
    syn::Type::Path(syn::TypePath { path, .. }) => {
      let ty_ident = &path.segments.first().unwrap().ident;
      match ty_ident.to_string().as_str() {
        "bool" => syn::Ident::new("Boolean", proc_macro2::Span::call_site()),
        "f32" => syn::Ident::new("Float", proc_macro2::Span::call_site()),
        _ => panic!("field type is not bool or float"),
      }
    }
    _ => panic!("field type is not bool"),
  }
}
fn capitalize(s: &str) -> String {
  let mut c = s.chars();
  match c.next() {
    None => String::new(),
    Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
  }
}

#[proc_macro_attribute]
pub fn animation_state(_content: TokenStream, input: TokenStream) -> TokenStream {
  animation_state_impl(input)
}
fn animation_state_impl(input: TokenStream) -> TokenStream {
  let item = parse_macro_input!(input as syn::Item);
  let (e, t) = match item {
    syn::Item::Enum(mut item) => {
      //variants to add: Enter, Exit, Any
      let variants_to_add: Vec<syn::Variant> = vec![quote! {Enter}, quote! {Exit}, quote! {Any}]
        .into_iter()
        .map(|v| syn::parse2(v).unwrap())
        .collect();

      let ident = item.ident.clone();
      item.variants.extend(variants_to_add);
      (
        syn::Item::Enum(item.clone()),
        quote! {
            impl AnimationState for #ident {
                fn enter_state() -> Self {
                    Self::Enter
                }
                fn exit_state() -> Self {
                    Self::Exit
                }
                fn any_state() -> Self {
                    Self::Any
                }
            }
        },
      )
    }
    _ => panic!("Only enums are supported"),
  };
  quote! {
      #e
      #t
  }
  .into()
}
#[derive(Clone)]
enum Arrow {
  #[allow(dead_code)]
  Then(Token![->]),
  #[allow(dead_code)]
  ThenWait((Token![|], Token![->])),
}
impl Parse for Arrow {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let lookahead = input.lookahead1();
    if lookahead.peek(Token![|]) {
      Ok(Self::ThenWait((input.parse()?, input.parse()?)))
    } else {
      Ok(Self::Then(input.parse()?))
    }
  }
}
#[derive(Clone)]

enum Condition {
  IsTrue(Ident),
  IsFalse((Token![!], Ident)),
}
impl Parse for Condition {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let lookahead = input.lookahead1();
    if lookahead.peek(Token![!]) {
      Ok(Self::IsFalse((input.parse()?, input.parse()?)))
    } else {
      Ok(Self::IsTrue(input.parse()?))
    }
  }
}
#[derive(Clone)]

struct Shift {
  shift: Ident,
}
impl Parse for Shift {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    Ok(Shift {
      shift: input.parse()?,
    })
  }
}
#[derive(Clone)]
enum After {
  Condition(Condition),
  Shift(Shift),
}
impl Parse for After {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let ident = input.call(Ident::parse)?;
    if ident == "when" {
      Ok(After::Condition(input.parse()?))
    } else if ident == "on" {
      let shift = Shift {
        shift: input.parse()?,
      };
      Ok(After::Shift(shift))
    } else {
      Err(syn::Error::new_spanned(
        ident,
        "should be either 'when' or 'on'",
      ))
    }
  }
}

struct Transition {
  start_state: Ident,
  arrow: Arrow,
  end_state: Ident,
  after: Option<After>,
}
impl Parse for Transition {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let mut transition = Transition {
      start_state: input.parse()?,
      arrow: input.parse()?,
      end_state: input.parse()?,
      after: None,
    };
    let lookahead = input.lookahead1();
    if lookahead.peek(Ident) {
      let after: After = input.parse()?;
      transition.after = Some(after)
    }
    Ok(transition)
  }
}
struct Transitions {
  transitions: Punctuated<Transition, Token![,]>,
}
impl Parse for Transitions {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    Ok(Transitions {
      transitions: input.parse_terminated(Transition::parse, Token![,])?,
    })
  }
}
#[proc_macro]
pub fn transitions(item: TokenStream) -> TokenStream {
  transitions_impl(item)
}
fn camelize(ident: Ident) -> Ident {
  use stringcase::Caser;
  let value = ident.to_string().to_pascal_case();
  let camel_text = value.as_str();
  Ident::new(camel_text, ident.span())
}
fn transitions_impl(item: TokenStream) -> TokenStream {
  let input = parse_macro_input!(item as Transitions);
  let transitions = input.transitions.iter().map(|transition| {
    let from = transition.start_state.clone();
    let to = transition.end_state.clone();
    let wait = matches!(transition.arrow, Arrow::ThenWait(_));
    let (condition, shift) = match transition.after.clone() {
      None => (quote!(None), quote!(None)),
      Some(After::Condition(cond)) => {
        let cond = match cond {
          Condition::IsTrue(ident) => {
            let camel_ident = camelize(ident);
            quote!(
                ::cockatiel::prelude::Condition::IsTrue(Vars::#camel_ident)
            )
          }
          Condition::IsFalse((_, ident)) => {
            let camel_ident = camelize(ident);

            quote!(
              ::cockatiel::prelude::Condition::IsFalse(Vars::#camel_ident)
            )
          }
        };
        (quote! {Some(#cond)}, quote!(None))
      }
      Some(After::Shift(Shift { shift })) => {
        let ident = camelize(shift);
        let shift = quote! {
            Shift::#ident
        };
        (quote!(None), quote!(Some(#shift)))
      }
    };
    quote! {
      ::cockatiel::prelude::Transition::new(State::#from, State::#to, #wait, #condition, #shift)
    }
  });
  let transition = quote! {
      vec![#(#transitions),*]
  };
  transition.into()
}
