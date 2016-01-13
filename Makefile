.PHONY: tests all clean

TESTDIR = tests

all_tests = $(shell ls $(TESTDIR)/test*)

all:
	ocamlbuild -use-ocamlfind main.byte

clean:
	ocamlbuild -clean

tests:
	for p in $(all_tests); do \
	ocamlbuild -use-ocamlfind $${p%.ml}.byte; \
	done
