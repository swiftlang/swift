// Associated types are declared in the protocol's module, but the typealias
// that witnesses them is synthesized in the conforming type's module and is
// never printed into the interface. Neither module can be named with a module
// selector in that position, so the printer must omit it.

// RUN: %empty-directory(%t)

// Build the module that declares the associated types.
// RUN: %target-swift-frontend -emit-module -swift-version 5 -enable-library-evolution -parse-as-library -module-name AssocTypeProto -o %t/AssocTypeProto.swiftmodule -emit-module-interface-path %t/AssocTypeProto.swiftinterface %S/Inputs/module_selector/assoc_type_proto.swift

// RUN: %target-swift-emit-module-interface(%t/TestCase.swiftinterface) %s -I %t -target %target-stable-abi-triple -module-name TestCase
// RUN: %FileCheck --input-file %t/TestCase.swiftinterface %s

// The interface has to be readable by the compiler that produced it.
// RUN: %target-swift-typecheck-module-from-interface(%t/TestCase.swiftinterface) -I %t -target %target-stable-abi-triple -module-name TestCase

// The same must hold when module selectors are turned off.
// RUN: %empty-directory(%t/disabled)
// RUN: %target-swift-emit-module-interface(%t/disabled/TestCase.swiftinterface) %s -I %t -target %target-stable-abi-triple -module-name TestCase -disable-module-selectors-in-module-interface
// RUN: %target-swift-typecheck-module-from-interface(%t/disabled/TestCase.swiftinterface) -I %t -target %target-stable-abi-triple -module-name TestCase

import AssocTypeProto

// CHECK-LABEL: public struct Bag<Element> :
// CHECK-SAME: AssocTypeProto::HasElement
public struct Bag<Element>: HasElement {
  public init() {}

  // The associated type is witnessed by 'Bag's generic parameter, so no module
  // selector belongs on 'Element' here.
  // CHECK: public func firstAssoc() ->
  // CHECK-SAME: TestCase::Bag<Element>.Element?
  // CHECK-NOT: .TestCase::Element
  // CHECK-NOT: .AssocTypeProto::Element
  public func firstAssoc() -> Self.Element? { nil }
}

// CHECK-LABEL: public struct Ints :
// CHECK-SAME: AssocTypeProto::HasIndex
public struct Ints: HasIndex {
  public typealias Index = Int

  public init() {}

  // An explicitly written type witness is printed into the interface, so it is
  // still nameable through the conforming type; the read-back RUN lines above
  // check that whatever the printer emits here parses.
  // CHECK: public func firstIndex() ->
  public func firstIndex() -> Self.Index? { nil }
}
