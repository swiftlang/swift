import Foo

func foo1() {
  var derivedObj = FooClassDerived()
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=cursor -pos=4:30 -cursor-action %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %sourcekitd-test -req=cursor -pos=4:30 -length=3 -cursor-action %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %sourcekitd-test -req=cursor -pos=4:20 -length=15 -cursor-action %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %sourcekitd-test -req=cursor -pos=4:20 -length=16 -cursor-action %s -- -F %S/Inputs/mock-sdk -I %t.tmp %s | %FileCheck %s -check-prefix=CHECK2

// CHECK1: ACTIONS BEGIN
// CHECK1-NEXT: source.refactoring.kind.rename.global
// CHECK1-NEXT: Global Rename
// CHECK1-NEXT: cannot rename a Clang symbol from its Swift reference

// CHECK2: ACTIONS BEGIN
// CHECK2-NEXT: ACTIONS END

// REQUIRES: OS=macosx || OS=linux-gnu
