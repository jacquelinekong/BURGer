#compiler for BURGer

#caml compilers
OCAMLBUILD = corebuild
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFLAGS = -use-ocamlfind -pkgs llvm,llvm.bitreader,llvm.linker,yojson
FLAGS = $(OCAMLFLAGS) -cflags -w,+a-4
OBJS = parser.cmo scanner.cmo calc.cmo

burger : $(OBJS)
	$(OCAMLC) -o burger $(OBJS)

scanner.ml : scanner.mll
	$(OCAMLLEX) scanner.mll

parser.ml parser.mli : parser.mly
	$(OCAMLYACC) parser.mly

%.cmo : %.ml
	$(OCAMLC) -c $<

%.cmi : %.mli
	$(OCAMLC) -c $<

.PHONY : clean
clean :
	rm -f burger parser.ml parser.mli scanner.ml *.cmo *.cmi

calc.cmo: scanner.cmo parser.cmi ast.cmi
calc.cmx: scanner.cmx parser.cmx ast.cmi
parser.cmo: ast.cmi parser.cmi
parser.cmx: ast.cmi parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmi
