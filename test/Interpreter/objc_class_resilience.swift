// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/libresilient_struct.%target-dylib-extension) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/libresilient_struct.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -o %t/main -Xlinker -rpath -Xlinker %t
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/libresilient_struct.%target-dylib-extension

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

runAllTests()
