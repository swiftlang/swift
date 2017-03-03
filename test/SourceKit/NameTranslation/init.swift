import Foo

func foo2 () {
  _ = FooClassBase(float: 2.3)
  _ = FooClassBase(float: 2.3, second: 2)
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=translate -objc-selector initWithFloat2: -pos=4:15 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=translate -objc-selector initWithFloat2 -pos=4:15 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s
// RUN: %sourcekitd-test -req=translate -objc-selector initWithFloat2:second2: -pos=5:15 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=translate -objc-selector initWithFloat2:second2:third: -pos=5:15 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s
// RUN: %sourcekitd-test -req=translate -objc-selector initFloat2:second2: -pos=5:15 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK2 %s

// CHECK-NONE: <empty name translation info>
// CHECK1: init(float2:)
// CHECK2: init(float2:second2:)
