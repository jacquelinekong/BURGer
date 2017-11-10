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
	rm -f burger parser.ml parser.mli scanner.ml *.cmo *.cmi parser.output

ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
burger.cmo: scanner.cmo parser.cmi codegen.cmo ast.cmo
burger.cmx: scanner.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo
