.PHONY: tests all clean compile testcg

TESTDIR = tests
TC=tigerc

all_tests = $(shell ls $(TESTDIR)/test*)
cg_tests = $(shell ls $(TESTDIR)/irgen/*.tig)

BIT=-m32

# install_libs are extracted from tiger.install file
install_libs = $(shell grep cma tiger.install | sed 's/^.*\/\(.*\)\.cma.*/\1.cma/g')

all:
	ocamlbuild -use-ocamlfind -yaccflag -v $(TC).byte
	ocamlbuild -use-ocamlfind -yaccflag -v $(install_libs)

clean:
	ocamlbuild -clean
	find . -name '*.s' -exec rm {} \;
	find . -name '*.out' -exec rm {} \;

tests:
	for p in $(all_tests); do \
	ocamlbuild -use-ocamlfind $${p%.ml}.byte; \
	./$$(basename $${p%.ml}.byte); \
	done

testbuild: all
	for t in $(cg_tests); do \
	./$(TC).byte -load $$t -codegen -p > $${t%.tig}.s 2>/dev/null; \
	test $$? -eq 0 && echo "passed: $$t" || echo "failed: $$t"; \
	done

testcg:
	passed=0; \
	total=0; \
	for t in $(cg_tests); do \
	f=$${t%.tig}.out; \
	total=$$((total+1)); \
	gcc -o $$f $(BIT) runtime/runtime.c $${t%.tig}.s 2>/dev/null && ./$$f 2>/dev/null && test $$? -eq 0 && passed=$$((passed+1)) && echo "passed: $$f" || echo "failed: $$f"; \
	done; \
	echo "passed: $$passed/$$total"
