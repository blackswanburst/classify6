classify6
=========

An OCAML command line tool to classify IPv6 addresses, and provide a bit of further information.

Compile with: 

To compile for bytecode for usage with ocamlrun:
ocamlc -g str.cma Classify6.ml -o classify6
or to compile natively:
ocamlopt str.cmxa Classify6.ml -o classify6

TODO: Add multicasts of different scopes and specificity.
Build environment to make the ocaml easily portable.
