top_srcdir = .

NAME = binocle

LINKED_WITH_TESTS =

TESTABLE_SOURCES = \
	Binocle.ml

BINOCLE_SOURCES = \
	Binocle.ml

BINOCLE_THREAD_SOURCES = \
	BinocleThread.ml

SOURCES = \
	$(BINOCLE_SOURCES)\
	$(BINOCLE_THREAD_SOURCES)

PACKAGES = batteries ppp.ppx net_codecs
THREAD_PACKAGES = net_codecs parsercombinator

INSTALLED = \
	META Binocle.cma $(SOURCES:.ml=.cmi) \
	Binocle.cmxa Binocle.a $(SOURCES:.ml=.cmx) \
	BinocleThread.cma \
	BinocleThread.cmxa BinocleThread.a

all: $(INSTALLED)

doc:

Binocle.cmxa: $(BINOCLE_SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a $(filter %.cmx, $^) -o $@

Binocle.cma: $(BINOCLE_SOURCES:.ml=.cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a $(filter %.cmo, $^) -o $@

Binocle.a: Binocle.cmxa

BinocleThread.cmxa: BinocleThread.cmx
	$(OCAMLOPT) -thread $(OCAMLOPTFLAGS) -a $(filter %.cmx, $^) -o $@

BinocleThread.cma: BinocleThread.cmo
	$(OCAMLC) -thread $(OCAMLFLAGS) -a $(filter %.cmo, $^) -o $@

BinocleThread.a: BinocleThread.cmxa

clean-spec:
	$(RM) $(wildcard *.cmxa *.cmx *.cma)

check-spec:

include $(top_srcdir)/make.common

# Dependencies

include .depend
