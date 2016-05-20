==========================================
Compiler for the Stop Programming Language
==========================================

This repo is based off a project I worked on as part of the COMS 4115: Programming Languages & Translators course taught by Professor Stephen A. Edwards at Columbia University. The aim of the project was to design and implement a functional, object-oriented language which compiled to the LLVM Intermediate Representation (LLVM IR). In implementing the compiler my team and I utilized a variety of tools including Ocamllex, Ocamlyacc, the LLVM Ocaml API, and the Jane Street Core Ocaml Libraries.

Original Contributors:
======================
James Maxwell Stenger, jms2431, jms2431@columbia.edu
Jillian Knoll, jak2246, jak2246@columbia.edu
Jonathan Elias Barios, jeb2239, jeb2239@columbia.edu
Lusa Zhan, lz2371, lz2371@columbia.edu

Attributions:
=============

Professors Stephen A. Edwards, MicroC Compiler, 
	available at http://www1.cs.columbia.edu/~sedwards/classes/2016/4115-spring/index.html

David Watkins et al., DICE: Distributed Systems Language, 
	available at http://www1.cs.columbia.edu/~sedwards/classes/2015/4115-fall/index.html

Jeff Lee, C Language Yacc Grammar, 
	available at https://www.lysator.liu.se/c/ANSI-C-grammar-y.html#translation-unit

Tool & API Documentation:
=========================

Ocamllex & Ocamlyacc:
	http://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html

LLVM Ocaml API:	
	https://www.lysator.liu.se/c/ANSI-C-grammar-y.html#translation-unit

Jane Street Core Libraries:
	https://ocaml.janestreet.com/ocaml-core/111.28.00/doc/core/

Ocaml Language References:
	https://realworldocaml.org/
	http://caml.inria.fr/pub/docs/manual-ocaml/language.html

Setup Instructions:
===================
https://github.com/realworldocaml/book/wiki/Installation-Instructions

Installation under Ubuntu 14.04 (Pulled from MicroC Setup Instructions)
-----------------------------------------------------------------------

The default LLVM package is 3.4, so we install the matching OCaml library
using opam.

sudo apt-get install m4 llvm llvm-devel

sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -y opam
opam init

eval `opam config env`

opam install llvm.3.4

Installation under OS X
-----------------------

Because of issues with installing the LLVM Ocaml Bindings in OPAM we recommend using a virtual development environment. 

The VM we used during our project is available at:
	https://onedrive.live.com/redir?resid=28C829D0789DE2F2!33649&authkey=!AOA6bnox_KJOxlM&ithint=file%2cgz

Installation: Linux
-------------------

Commands on Raspberry PI:



The default LLVM package is 3.4, so we install the matching OCaml library
using opam.

sudo apt-get install m4 llvm llvm-devel

sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -y opam
opam init

eval `opam config env`

opam install llvm.3.4


