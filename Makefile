.PHONY: all
all: verilog

.PHONY: verilog
verilog: app
	cabal run clash -- System --verilog

.PHONY: vhdl
vhdl: app
	cabal run clash -- System --vhdl

.PHONY: repl
repl: app
	cabal run clashi -- src/System.hs

.PHONY: app
app:
	$(MAKE) -C app

.PHONY: clean
clean:
	cabal clean
	rm -rf verilog/
	rm -rf vhdl/
	rm -f trace.txt

	$(MAKE) -C app clean
