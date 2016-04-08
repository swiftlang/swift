// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -c %S/../Inputs/resilient_protocol.swift -o %t/resilient_protocol.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -c %S/../Inputs/resilient_protocol.swift -o %t/resilient_protocol.o
// RUN: %target-build-swift %s -Xlinker %t/resilient_protocol.o -I %t -L %t -o %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

//
// Note: protocol resilience in the sense of resiliently adding new
// requirements with default implementations is actually tested in
// validation-test/Evolution/test_protocol_*.
//
// This test just ensures we can call materializeForSet defined in a
// protocol extension from a different resilience domain.
//

import StdlibUnittest


import resilient_protocol

var ResilientProtocolTestSuite = TestSuite("ResilientProtocol")

func increment(_ x: inout Int, by: Int) {
  x += by
}

struct OtherConformingType : OtherResilientProtocol { }

ResilientProtocolTestSuite.test("PropertyInProtocolExtension") {
  var o = OtherConformingType()

  increment(&o.propertyInExtension, by: 5)
  increment(&OtherConformingType.staticPropertyInExtension, by: 7)

  expectEqual(OtherConformingType.staticPropertyInExtension, 12)
}

runAllTests()
