# Basic sanity check that usage works.
#
# RUN: %{lit} --help > %t.out
# RUN: FileCheck < %t.out %s
#
# CHECK: Usage: lit.py [options] {file-or-path}
