# vCCS compiler
A compiler for the value-passing extension to Milner's Calculus of Communicating Systems (CCS)

It compiles vCCS (value-passing CCS) code into basic CCS source files

It has been developed as a project for the "Languages for Concurrency and Distribution" course from the Computer Science Master's Degree of the University of Padua (Unipd, Italy)

## Install
You may first need to install the OCaml language, with the [OPAM](https://opam.ocaml.org/) package-manager

Once OPAM is installed you can install the project by cloning this repo and installing it as a package with OPAM:
```bash
git clone https://github.com/albertolazari/vccs-compiler
( cd vccs-compiler; opam install . )
```

## Usage
Once installed you can compile a file `<file>.vccs` to `<file>.ccs` with
```bash
vcc <file>
```

You can set the value interval with
```bash
vcc -i <min>..<max> <file>
```

Use `-help` for more informations on usage
