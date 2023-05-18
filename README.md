`printer` attribute prints the name of the function when the function is called.

Options for the attribute (separated by comma)

```rust
#[printer($a, ?$b, $3, test, prefix = "ABC ", suffix = " DEF")]
fn foo(a: u32, b: u32, c: u32) -> u32 {
    a + b + c
}
```

It adds `#[cfg(test)] println!("ABC foo(a: {a}, b: {b:?}, c: {c}) DEF");` at the first line of `foo`.
`$a` means it should print the value of `a` using `Display` trait.
`?` at `?$b` means it uses `Debug` trait instead of `Display`. `$3` points the third argument, which is `c`.
If you want to print all the arguments simply write `all` or `?all`.

`test` adds `#[cfg(test)]`. There are 3 profiles available: `test`, `debug`, and `release`. If no profile is given, it works on all profiles.
Multiple profiles are also possible: `#[printer(test, debug)]` will add `#[cfg(all(test, debug_assertions))]` at the expanded result.

Don't use braces inside suffix and prefix.

If no option is given at all (`#[printer]`), it's equivalent to `#[printer(?all)]`.

```rust
#[printer(dump)]
fn foo() {}
```

`dump` option dumps the expanded macro to stdout when compiled. It only shows the dump when compiled, not when run.
It means you have to read the stdout after `cargo build`, not `cargo run`. `cargo run` works only when it's newly compiled.

```rust
#[printer(name = "foo")]
fn bar() {}
```

It uses the name `foo` instead of `bar`.

```rust
#[printer(eprintln)]
fn bar() {}
```

It uses `eprintln` instead of `println`. There're also `print` and `eprint`.