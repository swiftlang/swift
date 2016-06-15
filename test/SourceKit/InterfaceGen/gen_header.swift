// REQUIRES: objc_interop
// RUN: echo '#include "header.h"' > %t.m
// RUN: %sourcekitd-test -req=interface-gen -header %S/Inputs/header.h -- -fsyntax-only %t.m -I %S/Inputs > %t.response
// RUN: diff -u %s.response %t.response
// RUN: rm %t.m

// RUN: echo '#include "header2.h"' > %t.m
// RUN: %sourcekitd-test -req=interface-gen -header %S/Inputs/header2.h -- -fsyntax-only %t.m -I %S/Inputs > %t.header2.response
// RUN: diff -u %s.header2.response %t.header2.response
