// RUN: echo '#include "header.h"' > %t.m
// RUN: %sourcekitd-test -req=interface-gen -header %S/Inputs/header.h -- -fsyntax-only %t.m -I %S/Inputs > %t.response
// RUN: diff -u %s.response %t.response
