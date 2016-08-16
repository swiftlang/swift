import Foo

// REQUIRES: objc_interop

// Perform 8 concurrent cursor infos, which is often enough to cause
// contention.  We disable printing the requests to minimize delay.

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- \
// RUN:                  -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %clang-importer-sdk \
// RUN:   == -async -dont-print-request -req=cursor -pos=60:15 \
// RUN:   == -async -dont-print-request -req=cursor -pos=60:15 \
// RUN:   == -async -dont-print-request -req=cursor -pos=60:15 \
// RUN:   == -async -dont-print-request -req=cursor -pos=60:15 \
// RUN:   == -async -dont-print-request -req=cursor -pos=60:15 \
// RUN:   == -async -dont-print-request -req=cursor -pos=60:15 \
// RUN:   == -async -dont-print-request -req=cursor -pos=60:15 \
// RUN:   == -async -dont-print-request -req=cursor -pos=60:15 | %FileCheck %s -check-prefix=CHECK-FOO

// CHECK-FOO: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>FooRuncingOptions
// CHECK-FOO: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>FooRuncingOptions
// CHECK-FOO: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>FooRuncingOptions
// CHECK-FOO: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>FooRuncingOptions
// CHECK-FOO: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>FooRuncingOptions
// CHECK-FOO: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>FooRuncingOptions
// CHECK-FOO: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>FooRuncingOptions
// CHECK-FOO: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>FooRuncingOptions
