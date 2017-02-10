import Foo

var derivedObj = FooClassDerived()

func foo1(_ a : FooClassDerived) {
  _ = a.fooProperty1
  _ = a.fooInstanceFunc0()
}


// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=translate -objc-name FooClassDerived2 -pos=3:23 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=translate -objc-selector FooClassDerived2 -pos=3:23 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s
// RUN: %sourcekitd-test -req=translate -objc-name fooProperty2 -pos=6:16 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc1 -pos=7:16 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK3 %s

// CHECK1: FooClassDerived2
// CHECK-NONE: <empty name translation info>
// CHECK2: fooProperty2
// CHECK3: fooInstanceFunc1
