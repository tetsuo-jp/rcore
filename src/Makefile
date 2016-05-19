#OCAMLC=ocamlfind ocamlc -g -package extlib -linkpkg
#OCAMLC=ocamlfind ocamlopt -warn-error A -package extlib -linkpkg
OCAMLC=ocamlfind ocamlopt

all: ri

ri: BNFC_Util.ml AbsRcore.ml ShowRcore.ml PrintRcore.ml ParRcore.mli ParRcore.ml LexRcore.ml InvRcore.ml MacroRcore.ml EvalRcore.ml Program2DataRcore.ml Main.ml
	$(OCAMLC) -o $@ $^

LexRcore.ml: LexRcore.mll
	ocamllex LexRcore.mll

ParRcore.mli: ParRcore.mly
	ocamlyacc ParRcore.mly

BNFC_Util.ml: Rcore.cf
	bnfc -ocaml Rcore.cf
	touch BNFC_Util.ml

clean:
	-rm -f *.cmi *.cmo *.o *.mli *.cmx ri

distclean: clean
	-rm -f LexRcore.* ParRcore.* LayoutRcore.* SkelRcore.* PrintRcore.* ShowRcore.* TestRcore.* AbsRcore.* TestRcore BNFC_Util.ml\
	 	ri

install: ri
	cp -f ./ri ../web/

test:
	./ri ../examples/reverse.rcore ../examples/list.rcore_val
	./ri ../examples/skip.rcore ../examples/list.rcore_val
	./ri ../examples/move.rcore ../examples/list.rcore_val
	./ri ../examples/conditional.rcore ../examples/nil.rcore_val
	./ri ../examples/conditional.rcore ../examples/true.rcore_val
	./ri ../examples/stack.rcore ../examples/nil.rcore_val
