# Working in utop

`utop` is the enhanced REPL (Read-Eval-Print Loop) for OCaml. It is similar to Python's `ipython` or Haskell's `ghci`.

It has many features for testing and debugging your OCaml code efficiently. In this quick tutorial we share some tips on how to get the most out of it.

### Getting utop

utop is bundled as an [OCaml package](https://ocaml.org/p/utop/latest). Therefore, we recommend installing it using OCaml's
package manager `opam` rather than other available means (e.g. your distro's package manager).

[Once you have `opam` installed](https://opam.ocaml.org/doc/Install.html), utop can be pulled easily:

```shell
opam install utop
```

### Starting utop

Run utop from your terminal by just typing its name:

```
utop
```

If you have some OCaml code in a file called `foo.ml`, you can supply it via the `-init` option to utop.

```
utop -init foo.ml
```

This will compile the source code and import its definitions in the first context.

If you're working inside a dune project, running `dune utop` has the advantage of building the project beforehand so that its definition can be imported in utop.

> [!TIP]
> Syntax highlighting in utop isn't enabled by default.
> To enable colors, create a file called `.utoprc` in your home directory
> and paste the following settings in it - or make up your own colors:
> 
> ```
> profile:                  dark
> identifier.foreground:    none
> comment.foreground:       x-chocolate1
> doc.foreground:           x-light-salmon
> constant.foreground:      x-aquamarine
> keyword.foreground:       x-cyan1
> symbol.foreground:        x-cyan1
> string.foreground:        x-light-salmon
> char.foreground:          x-light-salmon
> quotation.foreground:     x-purple
> error.foreground:         x-red
> parenthesis.background:   blue
> ```

### Entering expressions

utop uses two semicolons `;;` to mark the end of an expression that's ready for evaluation. Pressing `Enter` after `;;` will submit the expression to the interpreter.

Until you add `;;` to an expression, pressing `Enter` will simply start a new line, and your expression will span multiple lines.

#### The completion bar

utop also features a completion bar at the bottom of the UI that suggests defined names as you type. You use `Alt+Left arrow` and `Alt+Right arrow` to move the focus to the desired suggestion and `Alt+Down arrow` to enter it in the prompt line.

> [!TIP]
> One of the best ways to get to know the language and discover the standard library (`List`, `String`, `Set`...) is to experiment with the suggested names and read their types.
> ```
> #show List.fold_left;;
> > val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
> ```

### Moving around

You can move the cursor, delete, copy and paste text in the prompt while editing an expression just like in any other REPL. Some less obvious inputs:

- The `Up arrow` and `Down arrow` keys let you browse through the history of the expressions you typed in during the current session and in past sessions.

- While editing a multiline expression, use `Ctrl+P` and `Ctrl+N` to the cursor move up and down a line, respectively.

- Like in bash, `Ctrl+A` and `Ctrl+E` jump to the start and the end of the line, and `Ctrl+U` clears any character before the cursor.

- To exit utop safely, press `Ctrl+D` **with an empty prompt line**.

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
let rec map f l = match l with [] -> [] | x :: l' -> f x :: map f l';;
> val map : ('a -> 'b) -> 'a list -> 'b list = <fun>

#trace map;;
> map is now traced.
```

The trace of `map succ [1;2;3]` will show a bunch of `<poly>`. Quite annoying.

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

One way to work around this is redefining the function with type annotations for the arguments
you want to inspect.

```ocaml
let rec map (f : int->int) l = match l with [] -> [] | x :: l' -> f x :: map f l';;
> val map : (int -> int) -> int list -> int list = <fun>

#trace map;;
> map is now traced.
```

Now we can make more sense of the trace output:

```ocaml
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

### Saving your session

If you wish to save your utop session to the file "foo.ml":

```
#utop_stash "foo.ml"
```

This writes correct OCaml code for the expressions that were successfully evaluated, with messages and errors of the toplevel wrapped in comments.

### Loading in existing code

To load in the contents of the OCaml `foo.ml` without leaving utop:

```
#use "foo.ml"
```

This will compile the file and add its definitions to the context.

### Using an .ocamlinit

There are some routine expressions that you wish would persist between utop sessions. Typing these expressions every time you quit and reenter utop can be quite tedious.

Fortunately, you can store this initialization code in a file called `.ocamlinit`. If utop is started in the same directory of this file, it will compile it and initialize itself with its contents.
