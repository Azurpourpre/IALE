# Industrial Assertion and Logic Extractor
Using Why3 to formally verify industrial programming.

**This project is still very new. It should not be used in production, proofs it provides cannot be trusted**

This contains two parts : a "compiler" that turns XML IEC 61131-3 project (as defined in IEC 61131-10) into Why3 term, and its assertion language that makes writing specification in a more "industrial" way.
## Dependencies 
To build this project you will need [OCaml](https://ocaml.org/) and its package manager [OPAM](https://opam.ocaml.org/).
```
opam install xml-light why3 re
```
You will also need to install a [prover supported by Why3](https://www.why3.org/doc/install.html#sec-provers).
```
why3 config detect
```
## Building
```
dune build
```

Additionaly, you can confirm that everything is working, by running the provided tests.
```
dune runtest
```
