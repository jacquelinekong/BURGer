#compiler for BURGer

#caml compilers
OCAMLBUILD = corebuild
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFLAGS = -use-ocamlfind -pkgs llvm,llvm.bitreader,llvm.linker,yojson
FLAGS = $(OCAMLFLAGS) -cflags -w,+a-4

TARGET = burger

.PHONY : $(TARGET).native

#.PHONY: clean

