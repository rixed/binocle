top_srcdir = .

NAME = binocle

LINKED_WITH_TESTS =

TESTABLE_SOURCES = \
	Binocle.ml

BINOCLE_SOURCES = \
	Binocle.ml

SOURCES = \
	$(BINOCLE_SOURCES)

PACKAGES = batteries ppp

INSTALLED = \
	META Binocle.cma $(BINOCLE_SOURCES:.ml=.cmi) \
	Binocle.cmxa Binocle.a $(BINOCLE_SOURCES:.ml=.cmx)

all: $(INSTALLED)

doc:

Binocle.cmxa: $(BINOCLE_SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a $(filter %.cmx, $^) -o $@

Binocle.cma: $(BINOCLE_SOURCES:.ml=.cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a $(filter %.cmo, $^) -o $@

Binocle.a: Binocle.cmxa

clean-spec:

distclean-spec:
	$(RM) Binocle.cmxa Binocle.cma Binocle.cmx

check-spec:

include $(top_srcdir)/make.common

# Dependencies

include .depend
