// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_protocol)) -enable-library-evolution %S/../Inputs/resilient_protocol.swift -emit-module -emit-module-path %t/resilient_protocol.swiftmodule -module-name resilient_protocol
// RUN: %target-codesign %t/%target-library-name(resilient_protocol)

// RUN: %target-build-swift %s -lresilient_protocol -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(resilient_protocol)

// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_protocol_wmo)) -enable-library-evolution %S/../Inputs/resilient_protocol.swift -emit-module -emit-module-path %t/resilient_protocol.swiftmodule -module-name resilient_protocol -whole-module-optimization
// RUN: %target-codesign %t/%target-library-name(resilient_protocol_wmo)

// RUN: %target-build-swift %s -lresilient_protocol_wmo -I %t -L %t -o %t/main2 %target-rpath(%t)
// RUN: %target-codesign %t/main2

// RUN: %target-run %t/main2 %t/%target-library-name(resilient_protocol_wmo)

// REQUIRES: executable_test

//
// Note: protocol resilience in the sense of resiliently adding new
// requirements with default implementations is actually tested in
// validation-test/Evolution/test_protocol_*.
//

import StdlibUnittest


import resilient_protocol

var ResilientProtocolTestSuite = TestSuite("ResilientProtocol")

func increment(_ x: inout Int, by: Int) {
  x += by
}

struct OtherConformingType : OtherResilientProtocol { }

// Ensure we can call materializeForSet defined in a protocol extension
// from a different resilience domain.
ResilientProtocolTestSuite.test("PropertyInProtocolExtension") {
  var o = OtherConformingType()

  increment(&o.propertyInExtension, by: 5)
  increment(&OtherConformingType.staticPropertyInExtension, by: 7)

  expectEqual(OtherConformingType.staticPropertyInExtension, 12)
}

struct DerivedConformingType : ResilientDerivedProtocol {
  func requirement() -> Int { return 42 }
}

// Ensure dynamic casts to resilient types work.
func callBaseRequirement(t: ResilientBaseProtocol) -> Int {
  return t.requirement()
}

@_optimize(none)
func castToDerivedProtocol<T>(t: T) -> Int {
  return callBaseRequirement(t: t as! ResilientDerivedProtocol)
}

ResilientProtocolTestSuite.test("DynamicCastToResilientProtocol") {
  expectEqual(castToDerivedProtocol(t: DerivedConformingType()), 42)
}

runAllTests()
