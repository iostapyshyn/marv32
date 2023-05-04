.PHONY: all
all: verilog

.PHONY: verilog
verilog:
	stack run clash -- System --verilog

.PHONY: vhdl
vhdl:
	stack run clash -- System --vhdl

.PHONY: repl
repl:
	stack run clashi

.PHONY: clean
clean:
	stack clean
	rm -rf verilog/
	rm -rf vhdl/
