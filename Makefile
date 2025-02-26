EMACS ?= emacs
BATCH = $(EMACS) --batch -Q
LOAD_PATH = -L .
ELS = consult-vc-modified-files.el
TESTS = consult-vc-modified-files-tests.el

.PHONY: all test clean 

all: test

test:
	@echo "Running tests..."
	@$(BATCH) $(LOAD_PATH) --eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	--eval "(package-initialize)" \
	--eval "(unless (package-installed-p 'consult) (package-refresh-contents) (package-install 'consult))" \
	-l ert -l $(ELS) -l $(TESTS) -f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning..."
	@rm -f *.elc
