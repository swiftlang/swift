import Foo

var derivedObj = FooClassDerived()

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=translate -objc-name FooClassDerived2 -pos=3:23 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=translate -objc-selector FooClassDerived2 -pos=3:23 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s

// CHECK1: FooClassDerived2
// CHECK-NONE: <empty name translation info>
