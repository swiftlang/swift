// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}resilient_struct%{target-shared-library-suffix}) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/%target-library-name(resilient_struct)

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -o %t/main -Xlinker -rpath -Xlinker %t
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(resilient_struct)

// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import resilient_struct


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

ResilientClassTestSuite.test("Category") {
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

runAllTests()
