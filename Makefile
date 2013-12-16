
all:	compile

compile:
	rebar compile

clean:
	rebar clean

raw-dot-tree: compile
	escript gen_dot_file dot raw | dot -Tpdf -o graph.pdf

.PHONY: raw-dot-tree dot-test compile
