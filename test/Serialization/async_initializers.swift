// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend  -disable-availability-checking -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck -check-prefix MODULE-CHECK %s
// RUN: %target-swift-frontend  -disable-availability-checking -emit-module-path %t/b.swiftmodule -module-name a %t/a.swiftmodule
// RUN: cmp -s %t/a.swiftmodule %t/b.swiftmodule

// REQUIRES: concurrency

///////////
// This test checks for correct serialization & deserialization of
// async initializers

// look for correct members in module's deserialization pretty-print:

// MODULE-CHECK:       actor A {
// MODULE-CHECK-NEXT:    init() async

actor A {
  init() async {}
}

// MODULE-CHECK:       class C {
// MODULE-CHECK-NEXT:    init() async
class C {
  init() async {}
}

// MODULE-CHECK:       struct S {
// MODULE-CHECK-NEXT:    init() async
struct S {
  init() async {}
}

// ignore-----MODULE-CHECK:       enum E {
// ignore-----MODULE-CHECK-NEXT:    case nothing
// ignore-----MODULE-CHECK-NEXT:    init() async

// FIXME: until rdar://76678907 is fixed, this won't work.
// enum E {
//   case nothing
//   init() async {
//     self = .nothing
//   }
// }
