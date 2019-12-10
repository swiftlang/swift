// RUN: %empty-directory(%t)

// RUN: %target-clang -fobjc-arc %S/../Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_struct)) -enable-library-evolution %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/%target-library-name(resilient_struct)

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -I %S/../Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(resilient_struct)

// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import resilient_struct
import ObjCClasses


var ResilientClassTestSuite = TestSuite("ResilientClass")

class ResilientFieldWithCategory {
  var value: ResilientInt?
}

@objc protocol MyProtocol {
  func myMethod() -> Int
}

extension ResilientFieldWithCategory : MyProtocol {
  @objc func myMethod() -> Int { return 42 }
}

func takesMyProtocol(_ p: MyProtocol) -> Int {
  return p.myMethod()
}

ResilientClassTestSuite.test("Category")
  .xfail(.osxMinor(10, 9, reason:
         "Category attachment with ARCLite on 10.9 doesn't work currently"))
  .code {
  expectEqual(42, takesMyProtocol(ResilientFieldWithCategory()))
}

// rdar://problem/45569020 - Make sure we initialize the superclass first
class ResilientSuperclass {
  var value: ResilientInt?
}

class ResilientSubclass : ResilientSuperclass {}

ResilientClassTestSuite.test("Superclass") {
  _blackHole(ResilientSubclass())
}

// rdar://48031465 - Make sure we handle sliding empty ivars properly.
struct Empty {}

class ClassWithEmptyThenResilient : HasHiddenIvars {
  let empty: Empty
  let resilient: ResilientInt

  init(empty: Empty, resilient: ResilientInt) {
    self.empty = empty
    self.resilient = resilient
  }
}

ResilientClassTestSuite.test("EmptyThenResilient") {
  let c = ClassWithEmptyThenResilient(empty: Empty(),
                                      resilient: ResilientInt(i: 17))
  c.x = 100
  c.y = 2000
  c.z = 30000
  c.t = 400000
  expectEqual(c.resilient.i, 17)
  expectEqual(c.x, 100)
  expectEqual(c.y, 2000)
  expectEqual(c.z, 30000)
  expectEqual(c.t, 400000)
}

class ClassWithResilientThenEmpty : HasHiddenIvars {
  let resilient: ResilientInt
  let empty: Empty

  init(empty: Empty, resilient: ResilientInt) {
    self.empty = empty
    self.resilient = resilient
  }
}

ResilientClassTestSuite.test("ResilientThenEmpty") {
  let c = ClassWithResilientThenEmpty(empty: Empty(),
                                      resilient: ResilientInt(i: 17))
  c.x = 100
  c.y = 2000
  c.z = 30000
  c.t = 400000
  expectEqual(c.resilient.i, 17)
  expectEqual(c.x, 100)
  expectEqual(c.y, 2000)
  expectEqual(c.z, 30000)
  expectEqual(c.t, 400000)
}

runAllTests()
