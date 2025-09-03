.PHONY: $(MAKECMDGOALS)

test:
	dune build testsuite && dune runtest
