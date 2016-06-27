NAME=udoc
TARGET=byte

all:
	ocamlbuild -use-ocamlfind -use-menhir src/$(NAME).$(TARGET)
	ocamlbuild -use-ocamlfind -use-menhir -package str coq2html.$(TARGET)

clean:
	ocamlbuild -clean
