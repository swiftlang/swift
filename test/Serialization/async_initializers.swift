// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend  -target %target-swift-5.1-abi-triple -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck -check-prefix MODULE-CHECK %s

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

// MODULE-CHECK:       enum E {
// MODULE-CHECK-NEXT:    case nothing
// MODULE-CHECK-NEXT:    init() async
enum E {
  case nothing
  init() async {
    self = .nothing
  }
}

// MODULE-CHECK:       struct S {
// MODULE-CHECK-NEXT:    init() async
struct S {
  init() async {}
}
