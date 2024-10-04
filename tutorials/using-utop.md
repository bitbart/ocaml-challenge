# Working in utop

`utop` is the REPL for OCaml, a tool similar to Python's `ipython` or Haskell's `ghci`.

It has many features for testing and debugging your OCaml code efficiently.

In this quick tutorial we share some tips to get the most out of it.

### Getting utop

utop comes as an OCaml package. Therefore, we recommend installing it using OCaml's
package manager `opam` rather than other available means (e.g. your distro's package manager).

[Once you have `opam` installed](https://opam.ocaml.org/doc/Install.html), utop can be pulled easily:

```shell
opam install utop
```

### Invoking utop

Run `utop` from your terminal by just typing its name.

```
utop
```

If you have some OCaml code in a file called `foo.ml` you can supply it via the `-init` option to have its definitions available inside utop.

```
utop -init foo.ml
```

If you're inside a dune project, running `dune utop` has the advantage of building the project beforehand so that its definition can be imported in utop.

### Tracing recursion

Maybe the most useful feature of utop is the ability to _trace_ recursive computations. The `#trace` command lets you inspect the step-by-step behavior of a recursive function call.

```ocaml
let rec fact n = if n <= 0 then 1 else n * fact (n-1);;
> val fact : int -> int = <fun>

#trace fact;;
> fact is now traced.
```

Now, every time `fact` is called on an `int`, utop prints every recursive subcalls (`<--`) and its partial result (`-->`). An asterisk `*` indicates that the function has been fed all its arguments and enters its body.

```
fact 3;;
> fact <-- 3
> fact <-- 2
> fact <-- 1
> fact <-- 0
> fact --> 1
> fact --> 1
> fact --> 2
> fact --> 6
> - : int = 6
```

#### Tracing polymorphic functions

There's a subtlety arising from tracing polymorphic functions: `#trace` won't refine the types of polymorphic parameters at runtime.

```ocaml
utop # let rec map f l = match l with [] -> [] | x :: l' -> f x :: map f l';;
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>

utop # #trace map;;
map is now traced.
```

The trace of `map succ [1;2;3]` will show a bunch of `<poly>`. This can be quite annoying.

```
utop # map succ [1;2;3];;
map <-- <fun>
map --> <fun>
map* <-- [<poly>; <poly>; <poly>]
map <-- <fun>
map --> <fun>
map* <-- [<poly>; <poly>]
map <-- <fun>
map --> <fun>
map* <-- [<poly>]
map <-- <fun>
map --> <fun>
map* <-- []
map* --> []
map* --> [<poly>]
map* --> [<poly>; <poly>]
map* --> [<poly>; <poly>; <poly>]
```

A way to work around this is redefining the function with type annotations for the arguments
you want to inspect.

```ocaml
let rec map (f : int->int) l = match l with [] -> [] | x :: l' -> f x :: map f l';;
> val map : (int -> int) -> int list -> int list = <fun>

#trace map;;
> map is now traced.

map succ [1;2;3];;
map <-- <fun>
map --> <fun>
map* <-- [1; 2; 3]
map <-- <fun>
map --> <fun>
map* <-- [2; 3]
map <-- <fun>
map --> <fun>
map* <-- [3]
map <-- <fun>
map --> <fun>
map* <-- []
map* --> []
map* --> [4]
map* --> [3; 4]
map* --> [2; 3; 4]
- : int list = [2; 3; 4]
```

### Saving your progress

If you wish to save your utop session to the file "foo.ml":

```
#utop_stash "foo.ml"
```

### Loading in existing code

To load in the contents of the OCaml `foo.ml` without leaving utop:

```
#use "foo.ml"
```

### Using an .ocamlinit

There are some routine expressions that you wish would persist between utop sessions.

Typing these expressions every time you quit and reenter utop can be quite tedious.

Fortunately, you can store these expressions in a file called `.ocamlinit`. If utop is started in the same directory, it will read the `.ocamlinit` and initialize itself with its contents.
