.PHONY: all
all: verilog

.PHONY: verilog
verilog:
	cabal run clash -- System --verilog

.PHONY: vhdl
vhdl:
	cabal run clash -- System --vhdl

.PHONY: repl
repl:
	cabal run clashi -- src/System.hs

.PHONY: clean
clean:
	cabal clean
	rm -rf verilog/
	rm -rf vhdl/
	rm -f trace.txt
