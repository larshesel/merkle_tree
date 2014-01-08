
all:	compile

test:	compile
	ERL_LIBS="../.." rebar eunit skip_deps=true

compile:
	rebar compile

clean:
	rebar clean

raw-dot-tree: compile
	escript gen_dot_file dot raw | dot -Tpdf -o graph.pdf

rm-backup-files:
	find . -name "*~" -exec rm {} \;

.PHONY: raw-dot-tree compile rm-backup-files all compile clean
