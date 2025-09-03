.PHONY: test
test:
	git clean -fxdq testsuite/artifacts
	dune b testsuite
	dune runt testsuite

