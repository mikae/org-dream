# Makefile for org-dream

VERSION="$(shell sed -nre '/^;;; Version:/ { s/^;;; Version:[ \t]+//; p }' org-dream.el)"
DISTFILE = org-dream-$(VERSION).zip

# EMACS value may be overridden
EMACS?=emacs
EMACS_MAJOR_VERSION=$(shell $(EMACS) -batch -eval '(princ emacs-major-version)')
ORG_DREAM_ELC=org-dream.$(EMACS_MAJOR_VERSION).elc

EMACS_BATCH=$(EMACS) --batch -Q

default:
	@echo version is $(VERSION)

%.$(EMACS_MAJOR_VERSION).elc: %.elc
	mv $< $@

%.elc: %.el
	$(EMACS_BATCH) -f batch-byte-compile $<

compile: $(ORG_DREAM_ELC)

.PHONY: test-compiled-nocask test-uncompiled-nocask test-compiled test-uncompiled
# check both regular and compiled versions
test-nocask: test-compiled-nocask test-uncompiled-nocask

test: test-compiled test-uncompiled

test-compiled-nocask: $(ORG_DREAM_ELC)
	$(EMACS) -batch -l $(ORG_DREAM_ELC) -l buttercup -f buttercup-run-discover

test-uncompiled-nocask:
	$(EMACS) -batch -l org-dream.el -l buttercup -f buttercup-run-discover

test-compiled: $(ORG_DREAM_ELC)
	EMACS=$(EMACS) cask exec buttercup -l $(ORG_DREAM_ELC)

test-uncompiled:
	EMACS=$(EMACS) cask exec buttercup -l org-dream.el
