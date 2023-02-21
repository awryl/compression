OCAMLC=ocamlc unix.cma
OCAMLOPT=ocamlopt unix.cmxa
OCAMLDEP=ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS= $(INCLUDES)    # add other options for ocamlc here

OBJS= burrows_wheeler.cmo movetofront.cmo huffman.cmo

huffman: $(OBJS) defaut.cmo main.cmo
	$(OCAMLC) -o huffman $(OCAMLFLAGS) defaut.cmo $(OBJS) main.cmo

distrib: defaut.cmo defaut.cmi defaut.mli $(OBJS:.cmo=.mli) $(OBJS:.cmo=.ml) main.cmo Makefile
	tar cvf distrib_huffman.tar defaut.cmo defaut.cmi defaut.mli $(OBJS:.cmo=.mli) $(OBJS:.cmo=.ml) main.cmo Makefile

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Clean up
clean:
	\rm -f huffman
	\rm -f $(OBJS)

# Dependencies
defaut.cmo: defaut.cmi
burrows_wheeler.cmo: defaut.cmi burrows_wheeler.cmi
movetofront.cmo: defaut.cmi movetofront.cmi
huffman.cmo: defaut.cmi huffman.cmi
main.cmo: movetofront.cmi huffman.cmi burrows_wheeler.cmi
