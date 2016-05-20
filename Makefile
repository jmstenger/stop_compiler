# Make sure ocamlbuild can find opam-managed packages: first run
# "$ eval `opam config env`"

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind

# See http://caml.inria.fr/pub/docs/manual-ocaml/comp.html for suppressed errors
# 44 & 45 In particular suppressed -- no errors, constantly filled up warning reports
stop:
	corebuild -use-ocamlfind -pkgs llvm,llvm.analysis \
		-cflags -w,+a-4-44-45 -I src/ stop.native

# "$ make clean" removes all generated files & test output files
.PHONY : clean
clean :
	corebuild -clean
	rm -rf *.diff stop scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.ll *.out *.log *.diff *.output

# "$ make clean_tests" removes all test output files
.PHONY : clean_tests
clean_tests :
	rm -f *.ll *.out *.log *.diff *.s

# "$ make parser" builds parser.mly so that ocamlyacc output can be observed
# (shift/reduce & reduce/reduce reported to stdout; details in parser.output)
.PHONY : parser
parser :
	ocamlyacc -v src/parser.mly

# "$ make clean_parser" cleans up files generated from parser.mly
.PHONY : clean_parser
clean_parser:
	rm src/parser.output src/parser.ml src/parser.mli
