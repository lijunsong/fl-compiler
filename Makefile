.PHONY: tests all clean

TESTDIR = tests

all_tests = $(shell ls $(TESTDIR)/test*)

all:
	ocamlbuild -I src -pkg Batteries main.byte

clean:
	ocamlbuild -clean

tests:
	for p in $(all_tests); do \
	ocamlbuild -I src -I tests -pkgs Batteries,OUnit $${p%.ml}.byte; \
	done
