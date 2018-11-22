.PHONY: test test-sbcl test-ccl test-ecl test-abcl pubdocs

heading_printer = $(shell which heading || echo 'true')
sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidocs = $(shell ls docs/*reference*.markdown)

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

# Documentation ---------------------------------------------------------------
$(apidocs): $(sourcefiles)
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs/build/index.html: $(docfiles) $(apidocs) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

docs: docs/build/index.html

pubdocs: docs
	hg -R ~/src/sjl.bitbucket.org pull -u
	rsync --delete -a ./docs/build/ ~/src/sjl.bitbucket.org/adopt
	hg -R ~/src/sjl.bitbucket.org commit -Am 'adopt: Update site.'
	hg -R ~/src/sjl.bitbucket.org push
