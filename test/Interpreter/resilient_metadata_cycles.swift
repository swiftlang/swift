// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/libresilient_struct.%target-dylib-extension) -Xfrontend -enable-resilience %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/libresilient_struct.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -o %t/main -Xlinker -rpath -Xlinker %t

// RUN: %target-run %t/main %t/libresilient_struct.%target-dylib-extension

import StdlibUnittest

import resilient_struct

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

runAllTests()
