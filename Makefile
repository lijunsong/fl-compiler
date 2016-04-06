.PHONY: tests all clean compile testcg

TESTDIR = tests

all_tests = $(shell ls $(TESTDIR)/test*)
cg_tests = $(shell ls $(TESTDIR)/irgen/*.tig)

all:
	ocamlbuild -use-ocamlfind -yaccflag -v main.byte

clean:
	ocamlbuild -clean

tests:
	for p in $(all_tests); do \
	ocamlbuild -use-ocamlfind $${p%.ml}.byte; \
	done

testbuild:
	for t in $(cg_tests); do \
	./main.byte -load $$t -codegen1 -p > $${t%.tig}.s; \
	done

testcg:
	for t in $(shell ls *.s); do \
	gcc -o $${t%.s}.out -m64 util/runtime.c $$t; \
	done
