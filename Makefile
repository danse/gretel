out = dist/build/gretel/gretel.jsexe
style = $(out)/style.css
logic = $(out)/out.js

all: $(style) $(logic)

style/style.css: style/Main.hs
	cd style && cabal run -v0 style > style.css

$(style): style/style.css
	cp $< $@

$(logic): Main.hs
	cabal build

