##===- utils/llvm-lit/Makefile -----------------------------*- Makefile -*-===##
# 
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
# 
##===----------------------------------------------------------------------===##

LEVEL = ../..

include $(LEVEL)/Makefile.common

# llvm-lit needs suffix.py for multiprocess to find a main module.
ifeq ($(HOST_OS),MingW)
  Suffix := .py
endif

all:: $(ToolDir)/llvm-lit$(Suffix)

$(ToolDir)/llvm-lit$(Suffix): llvm-lit.in Makefile $(ToolDir)/.dir
	$(Echo) "Creating 'llvm-lit' script..."
	$(Verb)$(ECHOPATH) s=@LLVM_SOURCE_DIR@=$(LLVM_SRC_ROOT)=g > lit.tmp
	$(Verb)$(ECHOPATH) s=@LLVM_BINARY_DIR@=$(LLVM_OBJ_ROOT)=g >> lit.tmp
	$(Verb)sed -f lit.tmp $< > $@
	$(Verb)chmod +x $@
	$(Verb)rm -f lit.tmp
