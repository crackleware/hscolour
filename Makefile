LIBRARY	= hscolour
VERSION	= 1.8

DIRS	= Language/Haskell/HsColour

SRCS	= Language/Haskell/HsColour.hs \
	  Language/Haskell/HsColour/General.hs \
	  Language/Haskell/HsColour/ANSI.hs \
	  Language/Haskell/HsColour/Anchors.hs \
	  Language/Haskell/HsColour/CSS.hs \
	  Language/Haskell/HsColour/Classify.hs \
	  Language/Haskell/HsColour/ColourHighlight.hs \
	  Language/Haskell/HsColour/Colourise.hs \
	  Language/Haskell/HsColour/HTML.hs \
	  Language/Haskell/HsColour/LaTeX.hs \
	  Language/Haskell/HsColour/TTY.hs

AUX	= README LICENCE* $(LIBRARY).cabal Setup.hs Makefile \
	  HsColour.hs hscolour.css .hscolour \
	  index.html docs/*

#all: $(LIBRARY)
executable: $(SRCS) HsColour.hs
	ghc --make -o $(LIBRARY) HsColour
package:
	tar cf tmp.tar $(SRCS) $(AUX)
	mkdir $(LIBRARY)-$(VERSION)
	cd $(LIBRARY)-$(VERSION); tar xf ../tmp.tar
	tar zcf $(LIBRARY)-$(VERSION).tar.gz $(LIBRARY)-$(VERSION)
	zip -r $(LIBRARY)-$(VERSION).zip $(LIBRARY)-$(VERSION)
	rm -r tmp.tar $(LIBRARY)-$(VERSION)
haddock: $(SRCS)
	mkdir -p docs/$(LIBRARY)
	for dir in $(DIRS); do mkdir -p docs/$(LIBRARY)/$$dir; done
	for file in $(SRCS); \
	    do ./HsColour -html -anchor $$file \
	          >docs/$(LIBRARY)/`dirname $$file`/`basename $$file .hs`.html;\
	    done
	haddock --html --title=$(LIBRARY) --odir=docs/$(LIBRARY) \
	    --package=$(LIBRARY) \
	    --source-module="%{MODULE/.//}.html" \
	    --source-entity="%{MODULE/.//}.html#%{NAME}" \
	    $(SRCS)

$(LIBRARY): $(SRCS)
	$(HC) $(HFLAGS) $(HEAP) -o $@  $(SRCS)
	$(STRIP) $@
