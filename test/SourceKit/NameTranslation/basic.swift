import Foo

var derivedObj = FooClassDerived()

func foo1(_ a : FooClassDerived) {
  _ = a.fooProperty1
  _ = a.fooInstanceFunc0()
  fooFunc3(1,1,1,nil)
}

func foo2 (_ a : FooClassDerived) {
  a.fooBaseInstanceFuncOverridden()
  a.fooInstanceFunc0()
  a.fooInstanceFunc1(2)
  a.fooInstanceFunc2(2, withB: 2)
  _ = a.fooProperty1
  _ = FooClassBase(float: 2.3)
  _ = FooClassBase()
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=translate -objc-name FooClassDerived2 -pos=5:30 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=translate -objc-selector FooClassDerived2 -pos=3:23 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK11 %s
// RUN: %sourcekitd-test -req=translate -objc-name fooProperty2 -pos=6:16 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc1 -pos=7:16 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK3 %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooFunc3:d:d:d: -pos=8:4 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s

// RUN: %sourcekitd-test -req=translate -objc-selector fooBaseInstanceFuncOverridden1 -pos=12:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK4 %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc01 -pos=13:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK5 %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc11: -pos=14:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK6 %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc2:withBB: -pos=15:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK7 %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc21:withBB: -pos=15:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK8 %s
// RUN: %sourcekitd-test -req=translate -objc-name fooProperty11 -pos=16:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK9 %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc21:withBB:withC: -pos=15:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s
// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc21: -pos=15:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s

// RUN: %sourcekitd-test -req=translate -objc-selector fooInstanceFunc21: -pos=17:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK10 %s
// RUN: %sourcekitd-test -req=translate -objc-selector initWithfloat2: -pos=17:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK12 %s
// RUN: %sourcekitd-test -req=translate -objc-selector initWithfloat2:D: -pos=17:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s
// RUN: %sourcekitd-test -req=translate -objc-selector init: -pos=17:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK13 %s
// RUN: %sourcekitd-test -req=translate -objc-selector iit: -pos=17:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK13 %s
// RUN: %sourcekitd-test -req=translate -objc-selector init: -pos=18:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s
// RUN: %sourcekitd-test -req=translate -objc-selector NAME -pos=18:13 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK14 %s

// CHECK1: FooClassDerived2
// CHECK-NONE: <empty name translation info>
// CHECK2: fooProperty2
// CHECK3: fooInstanceFunc1
// CHECK4: fooBaseInstanceFuncOverridden1
// CHECK5: fooInstanceFunc01
// CHECK6: fooInstanceFunc11(_:)
// CHECK7: fooInstanceFunc2(_:withBB:)
// CHECK8: fooInstanceFunc21(_:withBB:)
// CHECK9: fooProperty11
// CHECK10: init(nstanceFunc21:)
// CHECK11: init(lassDerived2:)
// CHECK12: init(float2:)
// CHECK13: init(_:)
// CHECK14: init
