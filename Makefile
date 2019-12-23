.PHONY: test test-sbcl test-ccl test-ecl test-abcl test-clasp pubdocs

heading_printer = $(shell which heading || echo 'true')
sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidocs = $(shell ls docs/*reference*.markdown)

# Testing ---------------------------------------------------------------------
test: test-sbcl test-ccl test-ecl test-abcl test-clasp

test-sbcl:
	$(heading_printer) computer 'SBCL'
	time sbcl --load test/run.lisp

test-ccl:
	$(heading_printer) slant 'CCL'
	time ccl --load test/run.lisp

test-ecl:
	$(heading_printer) roman 'ECL'
	time ecl -load test/run.lisp

test-abcl:
	$(heading_printer) broadway 'ABCL'
	time abcl --load test/run.lisp

test-clasp:
	$(heading_printer) o8 'CLASP'
	time clasp --load test/run.lisp

# Documentation ---------------------------------------------------------------
$(apidocs): $(sourcefiles)
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs/build/index.html: $(docfiles) $(apidocs) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

docs: docs/build/index.html

pubdocs: docs
	hg -R ~/src/docs.stevelosh.com pull -u
	rsync --delete -a ./docs/build/ ~/src/docs.stevelosh.com/adopt
	hg -R ~/src/docs.stevelosh.com commit -Am 'adopt: Update site.'
	hg -R ~/src/docs.stevelosh.com push
