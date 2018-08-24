// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/libresilient_struct.%target-dylib-extension) -Xfrontend -enable-resilience %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/libresilient_struct.%target-dylib-extension

// RUN: %target-build-swift-dylib(%t/libresilient_class.%target-dylib-extension) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../Inputs/resilient_class.swift -emit-module -emit-module-path %t/resilient_class.swiftmodule -module-name resilient_class -I%t -L%t -lresilient_struct
// RUN: %target-codesign %t/libresilient_class.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -lresilient_class -o %t/main -Xlinker -rpath -Xlinker %t
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/libresilient_struct.%target-dylib-extension %t/libresilient_class.%target-dylib-extension

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
