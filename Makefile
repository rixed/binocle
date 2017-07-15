top_srcdir = .

NAME = binocle

LINKED_WITH_TESTS =

TESTABLE_SOURCES = \
	Binocle.ml

BINOCLE_SOURCES = \
	Binocle.ml

SOURCES = \
	$(BINOCLE_SOURCES)

PACKAGES =

INSTALLED = \
	META Binocle.cma \
	Binocle.cmxa Binocle.a $(BINOCLE_SOURCES:.ml=.cmx)

all: $(INSTALLED)

doc:

Binocle.cmxa: $(BINOCLE_SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a       $(filter %.cmx, $^) -o $@

Binocle.cma: $(BINOCLE_SOURCES:.ml=.cmo)
	$(OCAMLC)   $(OCAMLFLAGS)    -a       $(filter %.cmo, $^) -o $@

all_tests.opt: $(LINKED_WITH_TESTS:.ml=.cmx) $(TESTABLE_SOURCES:.ml=.cmx) all_tests.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkpkg -package qcheck $(filter %.cmx, $^) $(filter %.ml, $^) -o $@

clean-spec:

distclean-spec:
	$(RM) Binocle.cmxa Binocle.cma Binocle.cmx

check-spec: ringbuf_test.opt
	@./ringbuf_test.opt || echo "FAILURE"

include $(top_srcdir)/make.common

# Dependencies

include .depend
