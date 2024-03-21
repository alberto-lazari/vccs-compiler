# vCCS compiler
A compiler for the value-passing extension to Milner's Calculus of Communicating Systems (CCS)

It compiles vCCS (value-passing CCS) code into basic CCS source files

It has been developed as a project for the "Languages for Concurrency and Distribution" course from the Computer Science Master's Degree of the University of Padua (Unipd, Italy)

## Build
You can build the project with the Dune build system.
You may first need to install the OCaml language, with the [OPAM](https://opam.ocaml.org/) package-manager

Once OPAM is installed you can install Dune with
```bash
opam install dune
```

Then just build the project with
```bash
dune build
```
