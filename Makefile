.PHONY: tests all clean compile testcg

TESTDIR = tests
TC=tigerc

all_tests = $(shell ls $(TESTDIR)/test*)
cg_tests = $(shell ls $(TESTDIR)/irgen/*.tig)

all:
	ocamlbuild -use-ocamlfind -yaccflag -v $(TC).byte
	ocamlbuild -use-ocamlfind -yaccflag -v temp.cma

clean:
	ocamlbuild -clean
	find tests -name '*.s' -exec rm {} \;

tests:
	for p in $(all_tests); do \
	ocamlbuild -use-ocamlfind $${p%.ml}.byte; \
	done

testbuild: all
	for t in $(cg_tests); do \
	./$(TC).byte -load $$t -codegen1 -p > $${t%.tig}.s 2>/dev/null; \
	test $$? -eq 0 && echo "passed: $$t" || echo "failed: $$t"; \
	done

testcg:
	for t in $(cg_tests); do \
	f=$${t%.tig}.out; \
	gcc -o $$f -m64 util/runtime.c $${t%.tig}.s 2>/dev/null && ./$$f 2>/dev/null && test $$? -eq 0 && echo "passed: $$f" || echo "failed: $$f"; \
	done
