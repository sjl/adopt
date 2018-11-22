.PHONY: test test-sbcl test-ccl test-ecl test-abcl

heading_printer = $(shell which heading || echo 'true')

# Testing ---------------------------------------------------------------------
test: test-sbcl test-ccl test-ecl test-abcl

test-sbcl:
	$(heading_printer) computer 'SBCL'
	sbcl --load test/run.lisp

test-ccl:
	$(heading_printer) slant 'CCL'
	ccl --load test/run.lisp

test-ecl:
	$(heading_printer) roman 'ECL'
	ecl -load test/run.lisp

test-abcl:
	$(heading_printer) broadway 'ABCL'
	abcl --load test/run.lisp
