OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamlfind ocamldep

INCLUDES= -I src
OCAMLFLAGS=$(INCLUDES) -package core,bitstring,ppx_bitstring.ext,ppx_deriving_yojson -thread
OCAMLOPTFLAGS=$(INCLUDES) -package core,bitstring,ppx_bitstring.ext,ppx_deriving_yojson -thread

OBJECTS =				\
	src/NetML_Layer_UDP.cmo		\
	src/NetML_Layer_TCP.cmo		\
	src/NetML_Layer_IPv4.cmo	\
	src/NetML_Layer_Ethernet.cmo	\
	src/NetML_Layer.cmo		\
	src/NetML_PCap.cmo		\
	src/NetML.cmo			\
	src/PCap_parser.cmo

all: $(OBJECTS) 
	$(OCAMLC) -linkpkg -o src/parser $(OCAMLFLAGS) $(OBJECTS)

# Common rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

src/%.cmo: src/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

src/%.cmi: src/%.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

src/%.cmx: src/%.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f src/*.cm[iox] .depend

# Dependencies

depend:
	$(OCAMLDEP) $(INCLUDES) src/*.mli src/*.ml > .depend

-include .depend
