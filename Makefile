
# quick makefile to document how to do the various tasks


.PHONY : build
build :
	cabal build

.PHONY : test
test :
	time cabal run teaberry-dev --disable-optimization -- test --hide-successes --ansi-tricks=false

.PHONY : run-tour
run-tour :
	time cabal run teaberry --disable-optimization --  -f examples/tests/fulltests/tour.tea

# parserize : todo - name this better, it's a kind of check on the parse error messages
.PHONY : parserize
parserize :
	./parserize.sh  > error_messages_sample_new
	meld error_messages_sample error_messages_sample_new

.PHONY : test-coverage
test-coverage :
	cabal test --enable-coverage

# website + examples: todo

# clean

.PHONY : clean
clean :
	cabal clean

# cabal check and outdated

.PHONY : package-check
package-check :
	cabal check
	cabal update
	cabal outdated
