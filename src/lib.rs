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
///
/// It uses the name `foo` instead of `bar`.
///
/// ```rust
/// #[printer(eprintln)]
/// fn bar() {}
/// ```
///
/// It uses `eprintln` instead of `println`. There're also `print` and `eprint`.
///
/// ```rust
/// #[printer(cfg = "baz")]
/// fn foo() {}
/// ```
///
/// It works only when `#[cfg(feature = "baz")]`. You can mix it with test, debug, and release. You can use at most one flag.
///
/// ```rust
/// #[printer($a, cond(a > 3))]
/// fn foo(a: u32) -> u32 { a + 1 }
/// ```
///
/// It works only when `a > 3`. It wraps the print macro with an if statement.
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

    let mut printer = format!(
        "{}!(\"{}{func_name}({}){}\"{});",
        options.print_func.to_string(),
        options.prefix,
        args.iter().map(
            |(param, is_debug)| format!("{param}: {}{}{}", "{", if *is_debug { ":?" } else { "" }, "}")
        ).collect::<Vec<String>>().join(", "),
        options.suffix,
        args.iter().map(
            |(param, _)| format!(", {param}")
        ).collect::<Vec<String>>().concat(),
    );

    if let Some(c) = &options.condition {
        printer = format!("if {c} {} {printer} {}", '{', '}');
    }

    format!(
        "{}{} {printer} {}",
        profile_cfg(&options.profile),
        "{",
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

fn profile_cfg(profile: &(bool, bool, bool, Option<String>)) -> String {
    match profile {
        (true, true, true, None) => "".to_string(),
        (true, true, true, Some(s)) => format!("#[cfg({s})]"),
        (true, false, false, None) => "#[cfg(test)]".to_string(),
        (true, false, false, Some(s)) => format!("#[cfg(all(test, {s}))]"),
        (false, true, false, s) => {
            let s = match s { Some(s) => format!(", feature = \"{s}\""), _ => String::new() };

            format!("#[cfg(all(not(test), debug_assertions{s}))]")
        },
        (false, false, true, s) => {
            let s = match s { Some(s) => format!(", feature = \"{s}\""), _ => String::new() };

            format!("#[cfg(all(not(test), not(debug_assertions){s}))]")
        },
        (true, true, false, s) => {
            let s = match s { Some(s) => format!(", feature = \"{s}\""), _ => String::new() };

            format!("#[cfg(all(test, debug_assertions{s}))]")
        },
        (true, false, true, s) => {
            let s = match s { Some(s) => format!(", feature = \"{s}\""), _ => String::new() };

            format!("#[cfg(all(test, not(debug_assertions){s}))]")
        },
        (false, true, true, None) => "#[cfg(not(test))]".to_string(),
        (false, true, true, Some(s)) => format!("#[cfg(all(not(test), {s}))]"),
        (false, false, false, _) => unreachable!()
    }
}

fn parse_options(attr: TokenStream) -> AttrOption {

    // default option
    if attr.is_empty() {
        return AttrOption {
            profile: (true, true, true, None),
            all: (true, true),
            args: vec![],
            prefix: String::new(),
            suffix: String::new(),
            func_name: None,
            print_func: PrintFunc::PrintLn,
            condition: None,
            dump: false
        }
    }

    let mut profile = (false, false, false, None);  // (test, debug, release, cfg)
    let mut all = (false, false);
    let mut args = vec![];
    let mut prefix = String::new();
    let mut suffix = String::new();
    let mut curr_state = OptionParseState::Init;
    let mut is_debug = false;
    let mut dump = false;
    let mut print_func = PrintFunc::PrintLn;
    let mut func_name = None;
    let mut condition = None;

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

                    else if id == "println" || id == "print" || id == "eprintln" || id == "eprint" {

                        if is_debug {
                            panic!("`?` before `{id}` is invalid");
                        }

                        else {
                            print_func = PrintFunc::from_string(&id).unwrap();
                        }

                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }

                    else if id == "prefix" || id == "suffix" || id == "name" || id == "cfg" {

                        if is_debug {
                            panic!("`?` before `{id}` is invalid");
                        }

                        curr_state = OptionParseState::ExpectChar('=', Box::new(get_next_state_from_id(&id).unwrap()));
                    }

                    else if id == "dump" {
                        dump = true;
                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }

                    else if id == "cond" {

                        if is_debug {
                            panic!("`?` before `{id}` is invalid");
                        }

                        curr_state = OptionParseState::CondInit;
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
            OptionParseState::CfgInit => match &token {
                TokenTree::Literal(l) => match strip_quotes(&l.to_string()) {
                    Some(s) => {
                        profile.3 = Some(s);
                        curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                    }
                    _ => panic!("A cfg name has to be quoted!")
                }
                _ => panic!("A cfg name is expected, but found {:?} instead", token.to_string())
            },
            OptionParseState::CondInit => match &token {
                TokenTree::Group(g) => {
                    condition = Some(g.stream().to_string());
                    curr_state = OptionParseState::ExpectChar(',', Box::new(OptionParseState::Init));
                }
                _ => panic!("A condition is expected, but not found")
            }
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

    if !profile.0 & !profile.1 & !profile.2 {  // profile == (false, false, false, _)
        profile.0 = true;
        profile.1 = true;
        profile.2 = true;
    }

    AttrOption {
        all,
        prefix,
        suffix,
        args,
        profile,
        func_name,
        print_func,
        condition,
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
    NameInit,
    CfgInit,
    CondInit,
}

struct AttrOption {
    prefix: String,
    suffix: String,
    func_name: Option<String>,
    profile: (bool, bool, bool, Option<String>),  // (test, debug, release, cfg)
    all: (bool, bool),  // (all, is_debug)
    args: Vec<(IndexOrName, bool)>,  // (arg, is_debug)
    print_func: PrintFunc,
    condition: Option<String>,
    dump: bool
}

enum IndexOrName {
    Index(usize),
    Name(String)
}

enum PrintFunc {
    PrintLn,
    Print,
    EPrintLn,
    EPrint,
}

impl PrintFunc {

    pub fn to_string(&self) -> String {
        match self {
            PrintFunc::PrintLn => "println",
            PrintFunc::Print => "print",
            PrintFunc::EPrintLn => "eprintln",
            PrintFunc::EPrint => "eprint",
        }.to_string()
    }

    pub fn from_string(s: &str) -> Option<Self> {
        match s {
            "println" => Some(PrintFunc::PrintLn),
            "print" => Some(PrintFunc::Print),
            "eprintln" => Some(PrintFunc::EPrintLn),
            "eprint" => Some(PrintFunc::EPrint),
            _ => None
        }
    }

}

fn strip_quotes(s: &str) -> Option<String> {

    if let Some(s) = s.strip_prefix('"') {

        if let Some(s) = s.strip_suffix('"') {
            return Some(s.to_string());
        }

    }

    None
}

fn get_next_state_from_id(id: &str) -> Option<OptionParseState> {

    let result = match id {
        "prefix" => OptionParseState::PrefixInit,
        "suffix" => OptionParseState::SuffixInit,
        "name" => OptionParseState::NameInit,
        "cfg" => OptionParseState::CfgInit,
        _ => { return None; }
    };

    Some(result)
}