// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -emit-module -emit-library -module-name protocol_descriptors -o %t/protocol_descriptors%{target-shared-library-suffix}
// RUN: %target-swift-reflection-dump -binary-filename %t/protocol_descriptors%{target-shared-library-suffix} | %FileCheck %s

// TODO: Linux support.
// UNSUPPORTED: linux

// CHECK: PROTOCOL DESCRIPTORS:
// CHECK: ---
// CHECK: name:{{ +}}FirstProtocol
// CHECK: numRequirementsInSignature: 0
// CHECK: numRequirements: 1
// CHECK: associatedTypeNames: ''
// CHECK: ...
protocol FirstProtocol {
  var i: Int { get }
}

// CHECK: ---
// CHECK: name:{{ +}}SecondProtocol
// CHECK: numRequirementsInSignature: 0
// CHECK: numRequirements: 2
// CHECK: associatedTypeNames: ''
// CHECK: ...
protocol SecondProtocol {
  var j: Int { get }
  var k: Int { get }
}

// CHECK: ---
// CHECK: name:{{ +}}KrakenProtocol
// CHECK: numRequirementsInSignature: 0
// CHECK: numRequirements: 4
// CHECK: associatedTypeNames: Toy1 Toy2
// CHECK: ...
protocol KrakenProtocol {
  associatedtype Toy1
  associatedtype Toy2

  var k: String { get }
  var k2: String { get }
}

// CHECK: ---
// CHECK: name:{{ +}}ChildOfTheKrakenProtocol
// CHECK: numRequirementsInSignature: 0
// CHECK: numRequirements: 3
// CHECK: associatedTypeNames: ToyM
// CHECK: ...
protocol ChildOfTheKrakenProtocol {
  associatedtype ToyM

  var k: Int { get }
  var k2: String { get }
}

// CHECK: ---
// CHECK: name:{{ +}}EmptyProtocol
// CHECK: numRequirementsInSignature: 0
// CHECK: numRequirements: 0
// CHECK: associatedTypeNames: ''
// CHECK: ...
protocol EmptyProtocol {

}
