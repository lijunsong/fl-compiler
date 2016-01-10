all: 
	ocamlbuild -I src -pkg Batteries main.native

clean:
	ocamlbuild -clean
