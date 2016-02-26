TARGET=byte
NAME=udoc

all:
	ocamlbuild -use-ocamlfind -use-menhir src/$(NAME).$(TARGET)

clean:
	ocamlbuild -clean
