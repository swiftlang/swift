// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/a.swiftmodule -module-name a %s -target %target-swift-5.1-abi-triple
// RUN: llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck -check-prefix MODULE-CHECK %s

///////////
// This test checks for correct serialization & deserialization of
// effectful properties and subscripts

// look for correct members in module's deserialization pretty-print:

// MODULE-CHECK:       actor A {
// MODULE-CHECK-NEXT:    var normalProp: Int
// MODULE-CHECK-NEXT:    var computedProp: Int { get }
// MODULE-CHECK-NEXT:    var asyncProp: Int { get async }

// MODULE-CHECK:       class C {
// MODULE-CHECK-NEXT:    var prop_asyncThrows: Int { get async throws }
// MODULE-CHECK-NEXT:    var prop_async: Int { get async }
// MODULE-CHECK-NEXT:    var prop_throws: Int { get throws }

// MODULE-CHECK:       enum E {
// MODULE-CHECK-NEXT:    subscript(e: Int) -> Int { get async throws }

// MODULE-CHECK:       protocol P {
// MODULE-CHECK-NEXT:    var propA: Int { get async }
// MODULE-CHECK-NEXT:    var propT: Int { get throws }
// MODULE-CHECK-NEXT:    var propAT: Int { get async throws }
// MODULE-CHECK-NEXT:    subscript(p: Int) -> Int { get async }
// MODULE-CHECK-NEXT:    subscript(p: Double) -> Int { get throws }
// MODULE-CHECK-NEXT:    subscript(p: String) -> Int { get async throws }

// MODULE-CHECK:       struct S {
// MODULE-CHECK-NEXT:    subscript(s: Int) -> Int { get async }
// MODULE-CHECK-NEXT:    subscript(s: Double) -> Int { get throws }

class C {
  var prop_asyncThrows : Int {
    get async throws { 0 }
  }

  var prop_async : Int {
    get async { 1 }
  }

  var prop_throws : Int {
    get throws { 2 }
  }
}

struct S {
  subscript(_ s : Int) -> Int {
    get async { 0 }
  }

  subscript(_ s : Double) -> Int {
    get throws { 0 }
  }
}

enum E {
  subscript(_ e : Int) -> Int {
    get async throws { 0 }
  }
}

actor A {
  var normalProp : Int = 0
  var computedProp : Int { get { 0 } }
  var asyncProp : Int {
    get async { 0 }
  }
}

protocol P {
   var propA : Int { get async }
   var propT : Int { get throws }
   var propAT : Int { get async throws }

   subscript(_ p : Int) -> Int { get async }

   subscript(_ p : Double) -> Int { get throws }

   subscript(_ p : String) -> Int { get async throws }
}
