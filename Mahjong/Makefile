MODULES=game player tile command main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip Mahjong.zip *.ml* *.mli *.md *.txt _tags Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)
	