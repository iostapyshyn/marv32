.PHONY: all
all: verilog

.PHONY: verilog
verilog:
	stack run clash -- CPU --verilog

.PHONY: vhdl
vhdl:
	stack run clash -- CPU --vhdl

.PHONY: repl
repl:
	stack run clashi

.PHONY: clean
clean:
	stack clean
	rm -rf verilog/
	rm -rf vhdl/
