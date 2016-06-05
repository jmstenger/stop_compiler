# The "Stop" Programming Language

This repository is based off a project I worked on as part of the COMS 4115: Programming Languages & Translators course taught by Professor Stephen A. Edwards at Columbia University. The aim of the project was to design and implement a functional, object-oriented language which compiled to the LLVM Intermediate Representation (LLVM IR). In implementing the compiler my team and I utilized a variety of tools including Ocamllex, Ocamlyacc, the LLVM Ocaml API, and the Jane Street Core Ocaml Libraries.

## Original Contributors:

James Maxwell Stenger, jms2431, jms2431@columbia.edu

Jillian Knoll, jak2246, jak2246@columbia.edu

Jonathan Elias Barios, jeb2239, jeb2239@columbia.edu

Lusa Zhan, lz2371, lz2371@columbia.edu

## Attributions:

Professor Stephen A. Edwards, MicroC Compiler, 
available on [the Spring 2016 Course Website](http://www1.cs.columbia.edu/~sedwards/classes/2016/4115-spring/index.html)

David Watkins et al., DICE: Distributed Systems Language, 
available on [the Fall 2015 Course Website](http://www1.cs.columbia.edu/~sedwards/classes/2015/4115-fall/index.html)

Jeff Lee, C Language Yacc Grammar, 
available [here](https://www.lysator.liu.se/c/ANSI-C-grammar-y.html)

## Tool & API Documentation:

Ocamllex & Ocamlyacc:

http://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html

LLVM Ocaml API:	 

https://www.lysator.liu.se/c/ANSI-C-grammar-y.html#translation-unit

Jane Street Core Libraries:  

https://ocaml.janestreet.com/ocaml-core/111.28.00/doc/core/

Ocaml Language References:

https://realworldocaml.org/

http://caml.inria.fr/pub/docs/manual-ocaml/language.html

## Setup Instructions:

### Installation under Ubuntu 16.04

The first step is to get Ocaml & OPAM installed. 
OPAM is a package manager for OCaml which we need in order to install external libraries like Core and the LLVM Ocaml bindings. 
Detailed instructions are available in [the Real World Ocaml Wiki](https://github.com/realworldocaml/book/wiki/Installation-Instructions)

When I installed the project, the process was:

	sudo add-apt-repository ppa:avsm/ppa
	sudo apt-get update
	sudo apt-get install curl build-essential m4 ocaml opam	
	
	opam init
	opam switch
	eval `opam config env`
	opam install core utop

(Note that you may need to add the "opam config env" line to your ~/.bashrc)

Having installed OCaml & OPAM, you now need to add dependencies specific to the project:

Install llvm using apt-get. 
During installation take note of which version of llvm is being installed and then install the appropriate bindings in OPAM. 
When I installed the project, apt-get installed llvm 3.8, so I installed the llvm 3.8 bindings in OPAM:

	sudo apt-get install llvm
	opam install llvm.3.8
