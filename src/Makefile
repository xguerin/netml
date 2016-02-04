OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamlfind ocamldep

INCLUDES=
OCAMLFLAGS=$(INCLUDES) -g -package core,bitstring,ppx_bitstring.ext,ppx_deriving_yojson -thread
OCAMLOPTFLAGS=$(INCLUDES) -g -package core,bitstring,ppx_bitstring.ext,ppx_deriving_yojson -thread

OBJECTS =				\
	NetML_Layer_IV_UDP.cmo		\
	NetML_Layer_IV_TCP.cmo		\
	NetML_Layer_IV.cmo		\
	NetML_Layer_III_IPv4.cmo	\
	NetML_Layer_III.cmo		\
	NetML_Layer_II_Ethernet.cmo	\
	NetML_Layer_II.cmo		\
	NetML_Layer.cmo			\
	NetML_PCap.cmo			\
	NetML.cmo			\
	PCap_parser.cmo

all: $(OBJECTS) 
	$(OCAMLC) -linkpkg -o parser $(OCAMLFLAGS) $(OBJECTS)

# Common rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f *.cm[iox] .depend

# Dependencies

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

-include .depend
