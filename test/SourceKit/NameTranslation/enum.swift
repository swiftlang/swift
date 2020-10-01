import Foo

func foo1() {
  _ = FooComparisonResult.orderedAscending
  _ = FooComparisonResult.orderedDescending
  _ = FooComparisonResult.orderedSame
  _ = FooRuncingOptions.enableMince
  _ = FooRuncingOptions.enableQuince
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=translate -objc-name orderedSome -pos=4:30 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=translate -objc-selector orderedSome -pos=4:30 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK-DIAG %s
// RUN: %sourcekitd-test -req=translate -objc-name enableThird -pos=7:30 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=translate -objc-name FooRuncingEnableThird -pos=7:30 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=translate -objc-name FooRuncinEnableThird -pos=7:30 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=translate -objc-name FooRinEnableThird -pos=7:30 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK3 %s

// CHECK1: orderedSome
// CHECK-DIAG: <empty name translation info; internal diagnostic: "Unable to resolve ObjC declaration name.">
// CHECK2: enableThird
// CHECK3: inEnableThird
