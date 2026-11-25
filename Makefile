# Requer: ghc (com o pacote mtl), alex e happy.
BUILD = build
GHC   = ghc -isrc -i$(BUILD) -outputdir $(BUILD) -package mtl

tiger: $(wildcard src/*.hs) $(BUILD)/Lexer.hs $(BUILD)/Parser.hs
	$(GHC) src/Main.hs -o $@

$(BUILD)/Lexer.hs: src/Lexer.x
	@mkdir -p $(BUILD)
	alex $< -o $@

$(BUILD)/Parser.hs: src/Parser.y
	@mkdir -p $(BUILD)
	happy --ghc $< -o $@

$(BUILD)/irrun: test/IRRun.hs $(wildcard src/*.hs) $(BUILD)/Lexer.hs $(BUILD)/Parser.hs
	$(GHC) -package containers test/IRRun.hs -o $@

# Com MARS_JAR=/caminho/Mars4_5.jar os testes executam tambem o MIPS gerado.
test: tiger $(BUILD)/irrun
	./test/run.sh

clean:
	rm -rf $(BUILD) tiger

.PHONY: test clean
