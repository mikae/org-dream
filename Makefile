# Makefile for org-dream

VERSION="$(shell sed -nre '/^;;; Version:/ { s/^;;; Version:[ \t]+//; p }' org-dream.el)"
DISTFILE = org-dream-$(VERSION).zip

# EMACS value may be overridden
EMACS?=emacs

default:
	@echo version is $(VERSION)

test: test-uncompiled

test-uncompiled:
	EMACS=$(EMACS) cask exec buttercup -l org-dream.el
