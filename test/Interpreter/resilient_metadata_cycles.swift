// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_struct)) -enable-library-evolution %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/%target-library-name(resilient_struct)

// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_class)) -enable-library-evolution %S/../Inputs/resilient_class.swift -emit-module -emit-module-path %t/resilient_class.swiftmodule -module-name resilient_class -I%t -L%t -lresilient_struct
// RUN: %target-codesign %t/%target-library-name(resilient_class)

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -lresilient_class -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(resilient_struct) %t/%target-library-name(resilient_class)

// REQUIRES: executable_test

import StdlibUnittest

import resilient_struct
import resilient_class

var ResilientMetadataCycleTests = TestSuite("Resilient metadata cycle tests")

// SR-7876
enum test0_Node {
  case link(size: Size, children: [test0_Node])

  static func test() -> [test0_Node] {
      return []
  }
}

ResilientMetadataCycleTests.test("SR-7876") {
  _ = test0_Node.test()
}

class ConcreteChildOfGeneric : ResilientGenericOutsideParent<OtherClass> {}

class OtherClass : ConcreteChildOfGeneric {
  static func test() {}
}

ResilientMetadataCycleTests.test("ResilientClassCycle") {
  _ = OtherClass.test()
}


runAllTests()
