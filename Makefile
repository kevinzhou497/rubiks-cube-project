MODULES=terminal listCube command action gl/cube gl/shader gl/renderer gl/window \
gl/matrix arrayCube gl/framebuffer gl/rect gl/texture gl/msgLog cubeSearcher
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml) types.ml main.ml command_test.ml cube_test.ml action_test.ml 
MLIS=$(MODULES:=.mli) ui.mli cube_rep.mli controller.mli cubeUtils.ml gl/uiUtil.ml gl/util.ml cubeSolver.mli
TEST=test.byte
DEMO=demoMain.byte
MAIN=windowMain.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build: format
	$(OCAMLBUILD) -tag thread $(OBJECTS)

test:
	$(OCAMLBUILD) -tag thread $(TEST) && ./$(TEST) -runner sequential

demo:
	$(OCAMLBUILD) -tag thread $(DEMO) && OCAMLRUNPARAM=b ./$(DEMO)

main:
	$(OCAMLBUILD) -tag thread $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

mac:
	touch USING_MAC_OS
	make main

check: 
	@bash check.sh
	
finalcheck:
	@bash check.sh final

zip: 
	zip -r cube.zip *.ml* *.sh *.md _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	gl *.txt ml *.pt
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -I _build/gl -package ounit2 -package torch \
	-package tgls -package glfw-ocaml -package stb_image \
		-html -stars -d _doc.public $(MLIS)
		

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -I _build/gl -package ounit2 \
	-package torch -package tgls -package glfw-ocaml -package stb_image \
	-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private cube.zip USING_MAC_OS

format:
	ocamlformat --inplace *.mli *.ml gl/*.ml gl/*.mli
