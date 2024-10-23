# Setting up OCaml

This file contains minimal instructions to setup a local installation of OCaml on Linux.
See [here](https://ocaml.org/docs/up-and-running) for instructions on other OSs.

## Installing OCaml

First, install [opam](https://opam.ocaml.org/doc/Install.html), the OCaml official package manager. Assuming you're working on Ubuntu:
```bash
sudo apt install opam
```
Then, you must initialize opam. This installs OCaml and creates a default switch:
```bash
opam init --bare -a -y
```
Here we assume you will work on the default switch. To check that a switch actually exists:
```bash
opam switch list
```
In case the previous command shows an empty list, you must manually create a switch:
```bash
opam switch create alf ocaml-base-compiler
```
This creates a switch for the LIP course with the most up-to-date version of the OCaml compiler.

The following command updates environment variables, to make OCaml commands available on the current switch:
```bash
eval $(opam env)
```

Finally, install a few extra OCaml packages:
```bash
opam install -y dune ocaml-lsp-server odoc ocamlformat utop ppx_inline_test
```
In particular, this installation includes:
- [**dune**](https://dune.readthedocs.io/), a build system for OCaml projects, similar to make;
- [**utop**](https://opam.ocaml.org/blog/about-utop/), a REPL interface for OCaml;
- [**ppx_inline_test**](https://github.com/janestreet/ppx_inline_test), a tool for writing in-line tests in OCaml.

We will use these tools for all the projects of the ALF course.

If you plan to use the emacs editor, run:
```bash
opam user-setup install
```

## First project

As a preliminary step, fork the repository. All the coding activities of this lab must be performed on your fork.

To check that everything is installed correctly, we set up a first project (see [here](https://ocaml.org/docs/up-and-running#starting-a-new-project) for more detailed instructions).

Once you have forked the repository and cloned it, change directory to the `lip/basics` folder. 
Then, create a new project called `helloworld` using dune. Below, the lines starting with `>` contain the expected output of the given shell commands:
```
dune init project helloworld
> Success: initialized project component named helloworld
```
This command creates a directory `helloworld` with the following file structure:
```
helloworld/
├── dune-project
├── bin
│   └── dune
│   └── main.ml
├── lib
│   └── dune
├── test
│   ├── dune
│   └── helloworld.ml
└── helloworld.opam
```
To check that the OCaml installation was successful, try to build the project from the `helloworld` directory:
```bash
cd helloworld
dune build
```
If there are no errors, the output should be empty.
Run the project as follows:
```
dune exec helloworld
> Hello, World!
```
We will discuss the project structure in detail in the next exercise. For the moment, note that the `_build` directory contains the output of the `dune build` command. This includes the `main.exe` executable inside the `_build/default/bin/` subdirectory.

In this very first project, all the source code is in `./bin/main.ml`. For more complex projects, we will mainly write our source code in the `lib` directory.
