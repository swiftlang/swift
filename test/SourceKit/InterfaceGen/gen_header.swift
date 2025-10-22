// REQUIRES: objc_interop
// RUN: echo '#include "header.h"' > %t.m
// RUN: %sourcekitd-test -req=interface-gen -header %S/Inputs/header.h -- -fsyntax-only %t.m -I %S/Inputs > %t.response
// RUN: %diff -u %s.response %t.response
// RUN: rm %t.m

// RUN: echo '#include "header2.h"' > %t.m
// RUN: %sourcekitd-test -req=interface-gen -header %S/Inputs/header2.h -swift-version=4 -- -fsyntax-only %t.m -I %S/Inputs > %t.header2.response
// RUN: %diff -u %s.header2.response %t.header2.response

// RUN: echo '#include "header2.h"' > %t.m
// RUN: %sourcekitd-test -req=interface-gen -header %S/Inputs/header2.h -swift-version=5 -pass-version-as-string -- -fsyntax-only %t.m -I %S/Inputs > %t.header2.swift4.response
// RUN: %FileCheck -input-file %t.header2.swift4.response %s -check-prefix=SWIFT
// SWIFT: public func show_only_for_swift()

// RUN: echo '#include "header3.h"' > %t.m
// RUN: %sourcekitd-test -req=interface-gen -header %S/Inputs/header3.h -swift-version=5 -- -fsyntax-only %t.m -I %S/Inputs > %t.header3.response
// RUN: %diff -u %s.header3.response %t.header3.response
