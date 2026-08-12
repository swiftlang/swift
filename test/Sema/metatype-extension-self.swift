// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget: IUnknown { }

@com(interface: "20000000-0000-0000-0000-000000000002")
protocol IGizmo: IUnknown { associatedtype Element }

// A protocol metatype extension has no generic signature, and its members are
// static members of the protocol metatype.  They cannot reference 'Self' or the
// extended protocol's associated types: either would leave a member's interface
// type carrying a type parameter with no environment to map it into.  Both are
// diagnosed rather than left to crash while forming the interface type.
extension IWidget.Protocol {
  func retSelf() -> Self { fatalError() }
  // expected-error@-1 {{'Self' cannot be referenced in a protocol metatype extension}}

  func paramSelf(_ x: Self) { }
  // expected-error@-1 {{'Self' cannot be referenced in a protocol metatype extension}}

  var propSelf: Self { fatalError() }
  // expected-error@-1 {{'Self' cannot be referenced in a protocol metatype extension}}

  func selfMetatype() -> Self.Type { fatalError() }
  // expected-error@-1 {{'Self' cannot be referenced in a protocol metatype extension}}

  // Members that do not reference 'Self' are fine, including ones with their
  // own generic parameters.
  func ok() -> Int { 42 }
  func generic<U>(_ x: U) -> U { x }
}

extension IGizmo.Protocol {
  func retAssoc() -> Element { fatalError() }
  // expected-error@-1 {{associated type 'Element' cannot be referenced in a protocol metatype extension}}

  func paramAssoc(_ x: Element) { }
  // expected-error@-1 {{associated type 'Element' cannot be referenced in a protocol metatype extension}}
}
