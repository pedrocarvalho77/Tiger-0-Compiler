# Requer: ghc (com o pacote mtl), alex e happy.
BUILD = build
GHC   = ghc -isrc -i$(BUILD) -outputdir $(BUILD) -package mtl

tiger: src/Main.hs $(BUILD)/Lexer.hs $(BUILD)/Parser.hs
	$(GHC) src/Main.hs -o $@

$(BUILD)/Lexer.hs: src/Lexer.x
	@mkdir -p $(BUILD)
	alex $< -o $@

$(BUILD)/Parser.hs: src/Parser.y
	@mkdir -p $(BUILD)
	happy --ghc $< -o $@

# Interm e TigerMips ainda não são usados pelo Main: só verificamos que compilam.
test: tiger
	$(GHC) -fno-code src/Interm.hs src/TigerMips.hs
	./test/run.sh

clean:
	rm -rf $(BUILD) tiger

.PHONY: test clean
