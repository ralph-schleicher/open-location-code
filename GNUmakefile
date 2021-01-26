## GNUmakefile --- Open Location Code library.

# Copyright (C) 2019 Ralph Schleicher

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#
#    * Neither the name of the copyright holder nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

## Code:

PACKAGE = open-location-code
VERSION = 2.1

### Rules

%: %.in
	sed -e 's/@PACKAGE@/$(PACKAGE)/g' \
	    -e 's/@VERSION@/$(VERSION)/g' $< > $@~ && mv -f $@~ $@

%.html: %.md
	markdown $< > $@~ && mv -f $@~ $@

.PHONY: all
all: $(PACKAGE).asd

.PHONY: clean
clean:
	rm -f $(PACKAGE).asd

.PHONY: check
check: all
	quicklisp-check-build -sbcl -ccl $(PACKAGE)
	sbcl --non-interactive --load tests.lisp

### Maintenance

.PHONY: t
t:
	cd open-location-code.git && git pull
	cp -p open-location-code.git/test_data/*.csv t
	svn status --verbose t

.PHONY: doc
doc:
	sbcl --non-interactive --load generate-doc.lisp

.PHONY: sync
sync: all
	~/src/github/github.sh open-location-code

## GNUmakefile ends here
