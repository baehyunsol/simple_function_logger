extern crate proc_macro;
use proc_macro::{TokenStream, TokenTree, Delimiter, Group};
use std::str::FromStr;

/// It prints the name of the function when the function is called.
///
/// Options for the attribute (separated by comma)
///
/// ```rust
/// #[printer($a, ?$b, $3, test, prefix = "ABC ", suffix = " DEF")]
/// fn foo(a: u32, b: u32, c: u32) -> u32 {
///     a + b + c
/// }
/// ```
///
/// It adds `#[cfg(test)] println!("ABC foo(a: {a}, b: {b:?}, c: {c}) DEF");` at the first line of `foo`.
/// `$a` means it should print the value of `a` using `Display` trait.
/// `?` at `?$b` means it uses `Debug` trait instead of `Display`. `$3` points the third argument, which is `c`.
/// If you want to print all the arguments simply write `all` or `?all`.
///
/// `test` adds `#[cfg(test)]`. There are 3 profiles available: `test`, `debug`, and `release`. If no profile is given, it works on all profiles.
/// Multiple profiles are also possible: `#[printer(test, debug)]` will add `#[cfg(all(test, debug_assertions))]` at the expanded result.
///
/// Don't use braces inside suffix and prefix.
///
/// If no option is given at all (`#[printer]`), it's equivalent to `#[printer(?all)]`.
///
/// ```rust
/// #[printer(dump)]
/// fn foo() {}
/// ```
///
/// `dump` option dumps the expanded macro to stdout when compiled. It only shows the dump when compiled, not when run.
/// It means you have to read the stdout after `cargo build`, not `cargo run`. `cargo run` works only when it's newly compiled.
///
/// ```rust
/// #[printer(name = "foo")]
/// fn bar() {}
/// ```
/// It uses the name `foo` instead of `bar`.
#[proc_macro_attribute]
pub fn printer(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut result = vec![];
    let mut curr_state = State::Init;
    let mut func_name = String::new();
    let mut args = vec![];

    let options = parse_options(attr);

    for token in item.clone().into_iter() {

        match curr_state {
            State::Init => match &token {
                TokenTree::Ident(i) => {
                    let i_string = i.to_string();

                    if i_string == "fn" {
                        curr_state = State::FnInit;
                    }

                    else if i_string == "struct" || i_string == "enum" || i_string == "union" || i_string == "type" {
                        return item;
                    }

                }
                _ => {}
            }
            State::FnInit => match &token {
                TokenTree::Ident(i) => {
                    func_name = i.to_string();
                    curr_state = State::ParamInit;
                }
                _ => {}
            }
            State::ParamInit => match &token {
                TokenTree::Group(g) => match g.delimiter() {
                    Delimiter::Parenthesis => {
                        args = read_args(g.stream());
                        curr_state = State::BodyInit;
                    }
                    _ => {}
                }
                _ => {}
            }
            State::BodyInit => match &token {
                TokenTree::Group(g) => match g.delimiter() {
                    Delimiter::Brace => {
                        let mut print_name = TokenStream::from_str(&format!("{}", print_info(&func_name, &args, &options))).unwrap();
                        print_name.extend(g.stream());
                        let new_group = Group::new(Delimiter::Brace, print_name);
                        result.push(TokenTree::Group(new_group));

                        continue;
                    },
                    _ => {}  // not a function body
                }
                _ => {}
            }
        }

        result.push(token);
    }

    if options.dump {
        println!("{}", result.clone().into_iter().collect::<TokenStream>().to_string());
    }

    result.into_iter().collect()
}

fn read_args(stream: TokenStream) -> Vec<String> {
    let mut curr_state = ArgState::NameInit;
    let mut result = vec![];

    for token in stream.into_iter() {

        match curr_state {
            ArgState::NameInit => match &token {
                TokenTree::Ident(i) => {
                    let ident = i.to_string();

                    if ident != "mut" {  // in order to process `&mut self`
                        result.push(ident);
                        curr_state = ArgState::NameDone;
                    }

                }
                _ => {}
            }
            ArgState::NameDone => match &token {
                TokenTree::Punct(p) => {
                    let c = p.as_char();

                    if c == ',' {
                        curr_state = ArgState::NameInit;
                    }

                    else if c == '<' {
                        curr_state = ArgState::BracketWait;
                    }

                }
                _ => {}
            }
            ArgState::BracketWait => match &token {
                TokenTree::Punct(p) => if p.as_char() == '>' {
                    curr_state = ArgState::NameDone;
                } else {}
                _ => {}
            }
        }

    }

    result
}

fn print_info(func_name: &str, args: &[String], options: &AttrOption) -> String {
    let args = solve_args(args, options);

    let func_name = match &options.func_name {
        Some(f) => f.to_string(),
        _ => func_name.to_string()
    };

    format!(
        "{}{} println!(\"{}{func_name}({}){}\"{}); {}",
        profile_cfg(&options.profile),
        "{",
        options.prefix,
        args.iter().map(
            |(param, is_debug)| format!("{param}: {}{}{}", "{", if *is_debug { ":?" } else { "" }, "}")
        ).collect::<Vec<String>>().join(", "),
        options.suffix,
        args.iter().map(
            |(param, _)| format!(", {param}")
        ).collect::<Vec<String>>().concat(),
        "}"
    )
}

fn solve_args(args: &[String], options: &AttrOption) -> Vec<(String, bool)> {
    let mut result = Vec::with_capacity(options.args.len());

    if options.all.0 {
        return args.iter().map(|arg| (arg.to_string(), options.all.1)).collect()
    }

    for arg in options.args.iter() {

        match &arg.0 {
            IndexOrName::Index(i) => if *i > 0 && *i - 1 < args.len() {
                result.push((args[*i - 1].to_string(), arg.1));
            } else if *i == 0 {
                panic!("You have to use 1-based index, not 0-based");
            } else {
                panic!("You asked for {i}th argument, but it has only {} arguments: {args:?}", args.len());
            }
            IndexOrName::Name(name) => if args.contains(name) {
                result.push((name.to_string(), arg.1));
            } else {
                panic!("No argument named {name:?}")
            }
        }

    }

    result
}

fn profile_cfg(profile: &(bool, bool, bool)) -> String {
    match profile {
        (true, true, true) => String::new(),
        (true, false, false) => "#[cfg(test)]".to_string(),
        (false, true, false) => "#[cfg(all(not(test), debug_assertions))]".to_string(),
        (false, false, true) => "#[cfg(all(not(test), not(debug_assertions)))]".to_string(),
        (true, true, false) => "#[cfg(all(test, debug_assertions))]".to_string(),
        (true, false, true) => "#[cfg(all(test, not(debug_assertions)))]".to_string(),
        (false, true, true) => "#[cfg(not(test))]".to_string(),
        _ => unreachable!()
    }
}

fn parse_options(attr: TokenStream) -> AttrOption {

    // default option
    if attr.is_empty() {
        return AttrOption {
            profile: (true, true, true),
            all: (true, true),
            args: vec![],
            prefix: String::new(),
            suffix: String::new(),
            func_name: None,
            dump: false
        }
    }

    let mut profile = (false, false, false);  // (test, debug, release)
    let mut all = (false, false);
    let mut args = vec![];
    let mut prefix = String::new();
    let mut suffix = String::new();
    let mut curr_state = OptionParseState::Init;
    let mut is_debug = false;
    let mut dump = false;
    let mut func_name = None;

    for token in attr.clone().into_iter() {

        match curr_state {
            OptionParseState::Init => match &token {
                TokenTree::Ident(i) => {
                    let id = i.to_string();

                    if id == "all" {
                        all = (true, is_debug);
                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }

                    else if id == "test" || id == "debug" || id == "release" {

                        if is_debug {
                            panic!("`?` before `{id}` is invalid");
                        }

                        else if id == "test" {
                            profile.0 = true;
                        }

                        else if id == "debug" {
                            profile.1 = true;
                        }

                        else {
                            profile.2 = true;
                        }

                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }

                    else if id == "prefix" {
                        curr_state = OptionParseState::ExpectChar('=', Box::new(OptionParseState::PrefixInit));
                    }

                    else if id == "suffix" {
                        curr_state = OptionParseState::ExpectChar('=', Box::new(OptionParseState::SuffixInit));
                    }

                    else if id == "name" {
                        curr_state = OptionParseState::ExpectChar('=', Box::new(OptionParseState::NameInit));
                    }

                    else if id == "dump" {
                        dump = true;
                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }

                    else {
                        panic!("Unexpected keyword in a `printer` attribute: {:?}\nIf {id} is a name of an argument, prefix it with `$`.", token.to_string());
                    }

                },
                TokenTree::Punct(p) => {
                    let c = p.as_char();

                    if c == '?' {

                        if is_debug {
                            panic!("consecutive `?` in a `printer` attribute");
                        }

                        else {
                            is_debug = true;
                        }

                    }

                    else if c == '$' {
                        curr_state = OptionParseState::ArgInit;
                    }

                    else {
                        panic!("Unexpected token in a `printer` attribute: {:?}", token.to_string());
                    }

                }
                _ => panic!("Unexpected token in a `printer` attribute: {:?}", token.to_string())
            }
            OptionParseState::ArgInit => match &token {
                TokenTree::Ident(i) => {
                    args.push((IndexOrName::Name(i.to_string()), is_debug));
                    curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                }
                TokenTree::Literal(n) => match n.to_string().parse::<usize>() {
                    Ok(n) => {
                        args.push((IndexOrName::Index(n), is_debug));
                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }
                    _ => panic!("Unexpected token in a `printer` attribute: {:?}", token.to_string())
                }
                _ => panic!("Unexpected token in a `printer` attribute: {:?}", token.to_string())
            },
            OptionParseState::PrefixInit => match &token {
                TokenTree::Literal(l) => match strip_quotes(&l.to_string()) {
                    Some(s) => {
                        prefix = s;
                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }
                    _ => panic!("A prefix string has to be quoted!")
                }
                _ => panic!("A prefix string is expected, but found {:?} instead", token.to_string())
            },
            OptionParseState::SuffixInit => match &token {
                TokenTree::Literal(l) => match strip_quotes(&l.to_string()) {
                    Some(s) => {
                        suffix = s;
                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }
                    _ => panic!("A suffix string has to be quoted!")
                }
                _ => panic!("A suffix string is expected, but found {:?} instead", token.to_string())
            },
            OptionParseState::NameInit => match &token {
                TokenTree::Literal(l) => match strip_quotes(&l.to_string()) {
                    Some(s) => {
                        func_name = Some(s);
                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }
                    _ => panic!("A func-name has to be quoted!")
                }
                _ => panic!("A suffix string is expected, but found {:?} instead", token.to_string())
            },
            OptionParseState::ExpectChar(c, next_state) => match &token {
                TokenTree::Punct(p) if p.as_char() == c => {
                    curr_state = *next_state;
                    is_debug = false;
                }
                _ => panic!("`{c}` is expected, but not found")
            }
        }

    }

    match curr_state {
        OptionParseState::Init | OptionParseState::ExpectChar(',', _) => {}
        _ => {
            panic!("Unexpected end of a `printer` attribute");
        }
    }

    if profile == (false, false, false) {
        profile = (true, true, true);
    }

    AttrOption {
        all,
        prefix,
        suffix,
        args,
        profile,
        func_name,
        dump
    }
}

enum ArgState {
    NameInit,
    NameDone,
    BracketWait
}

enum State {
    Init,
    FnInit,
    ParamInit,
    BodyInit
}

enum OptionParseState {
    Init,
    ArgInit,
    ExpectChar(char, Box<OptionParseState>),
    PrefixInit,
    SuffixInit,
    NameInit
}

struct AttrOption {
    prefix: String,
    suffix: String,
    func_name: Option<String>,
    profile: (bool, bool, bool),  // (test, debug, release)
    all: (bool, bool),  // (all, is_debug)
    args: Vec<(IndexOrName, bool)>,  // (arg, is_debug)
    dump: bool
}

enum IndexOrName {
    Index(usize),
    Name(String)
}

fn strip_quotes(s: &str) -> Option<String> {

    if let Some(s) = s.strip_prefix('"') {

        if let Some(s) = s.strip_suffix('"') {
            return Some(s.to_string());
        }

    }

    None
}