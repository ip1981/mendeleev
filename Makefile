BINARIES := \
	mendeleev-c \
	mendeleev-c-cpp \
	mendeleev-f \
	mendeleev-hs \
	mendeleev-py \
	mendeleev-tree-c \

LISP := \
	clisp:mendeleev.lisp \
	ecl:--shell:mendeleev.lisp \
	sbcl:--script:mendeleev.lisp \

PYTHON := \
	pypy:mendeleev.py \
	python2:mendeleev.py \
	python3:mendeleev.py \

SCRIPTS := $(LISP) $(PYTHON)

.PHONY: build
build: $(BINARIES)

RM := rm -f -v
MV := mv -f -v
.PHONY: clean
clean:
	$(RM) $(BINARIES)
	$(RM) prof-*


# Testing
TEST_FILE := test.txt
WORDS := $(shell awk -F: '/:/ {print "\""$$1"\""}' $(TEST_FILE))
EMPTY :=
SPACE := $(EMPTY) $(EMPTY)
.PHONY: test
define mktest
test: test-$(subst $(SPACE),-,$(1))
.PHONY: test-$(subst $(SPACE),-,$(1))
test-$(subst $(SPACE),-,$(1)): $(lastword $(1))
	$(wordlist 2, $(words $(1)), _ $(1)) ./$(lastword $(1)) $(WORDS) | diff -u $(TEST_FILE) -
endef
$(foreach t,$(SCRIPTS),$(eval $(call mktest,$(subst :, ,$(t)))))
$(foreach t,$(BINARIES),$(eval $(call mktest,$(t))))


# Profiling
PROF_TEST := hehehehehehehehehehehehehehehehe
.PHONY: prof
prof: \
	prof-mendeleev-c.txt \
	prof-mendeleev-f.txt \
	prof-mendeleev-hs.txt \
	prof-mendeleev-py.txt \
	prof-mendeleev-tree-c.txt \

%.gmon: %
	$(RM) $<-gmon.*
	GMON_OUT_PREFIX=$<-gmon ./$< $(PROF_TEST) > /dev/null
	$(MV) $<-gmon.* $@


# C
CC = gcc
CFLAGS = -std=c99 -Wall -Wextra -O2
%-c: %.c
	$(CC) $(CFLAGS) $< -o $@

CFLAGS_PROF = -O0 -g -pg
prof-%-c: %.c
	$(CC) $(CFLAGS_PROF) $< -o $@

prof-%-c.txt: prof-%-c.gmon
	gprof --brief prof-$*-c $< > $@


# C++
CXX = g++
CXXFLAGS = -std=c++98 -Wall -Wextra -O2
%-c-cpp: %.c
	$(CXX) $(CXXFLAGS) $< -o $@


# Fortran
FC = gfortran
FFLAGS = -std=f2003 -Wall -Wextra -O2
mendeleev-f: mendeleev.f90
	$(FC) $(FFLAGS) $< -o $@

FFLAGS_PROF = -O0 -g -pg -fcheck=all
prof-mendeleev-f: mendeleev.f90
	$(FC) $(FFLAGS_PROF) $< -o $@

prof-mendeleev-f.txt: prof-mendeleev-f.gmon
	gprof --brief prof-mendeleev-f $< > $@


# Haskell
HC = ghc
HCFLAGS = -XHaskell98 -no-keep-hi-files -no-keep-o-files -Wall -O2
mendeleev-hs: mendeleev.hs
	$(HC) $(HCFLAGS) $< -o $@

HCFLAGS_PROF = -prof -fprof-auto -rtsopts -no-keep-hi-files -no-keep-o-files
prof-mendeleev-hs: mendeleev.hs
	$(HC) $(HCFLAGS_PROF) $< -o $@

prof-mendeleev-hs.txt: prof-mendeleev-hs
	./$< +RTS -p -RTS $(PROF_TEST) > /dev/null
	$(MV) $<.prof $@


# Python
NUITKA = nuitka3
NUITKA_FLAGS = --quiet --remove-output --follow-imports
mendeleev-py: mendeleev.py
	$(NUITKA) $(NUITKA_FLAGS) $< -o $@

prof-mendeleev-py.dat: mendeleev.py
	python3 -m cProfile -o $@ $< $(PROF_TEST) > /dev/null

prof-mendeleev-py.txt: prof-mendeleev-py.dat
	python3 -c 'import pstats; pstats.Stats("$<").sort_stats("tottime").print_stats()' > $@


