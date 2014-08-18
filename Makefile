OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = -classic-display -use-ocamlfind

byte:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) src/tortu.byte

native:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) src/tortu.native

clean:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -clean

.PHONY: byte native clean
