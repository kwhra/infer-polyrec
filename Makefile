#compile options
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

# make "infer"
# infer type of testcase in test.ml 
# The list of object files for "infer"
PROG1_OBJS=syntax.cmo basic_op.cmo variable.cmo print.cmo print_latex.cmo\
					 unify.cmo inferlog.cmo infertype.cmo test.cmo infertype_client.cmo
PROG1 = infer

$(PROG1): $(PROG1_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $(PROG1_OBJS)

# Common rules
.SUFFIXES: .ml .cmo .cmi

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Clean up
clean:
	rm -f $(PROG1)
	rm -f *.cm[io]

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend