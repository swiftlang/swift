// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM

@_marker
protocol LocalMarker {}

protocol SwiftProtocol {
  func requirement()
}

class NativeBase {}

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IBase {}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IDerived: IBase {}

@com(interface: "20000000-0000-0000-0000-000000000001")
protocol IIndependent {}

func acceptInterface(_: any IDerived) {}
func acceptMarked(_: any IDerived & Sendable & LocalMarker) {}
func acceptRefinementChain(_: any IDerived & IBase) {}

// A generic environment can carry independent COM conformances separately.

func acceptGeneric<T: IDerived & IIndependent>(_: T) {}

func rejectIndependent(_: any IDerived & IIndependent) {}
// expected-error@-1 {{COM existential cannot contain interfaces 'IDerived' and 'IIndependent' because neither refines the other}}

func rejectSwiftProtocol(_: any IDerived & SwiftProtocol) {}
// expected-error@-1 {{COM existential containing interface 'IDerived' cannot also contain non-marker protocol 'SwiftProtocol'}}

func rejectAnyObject(_: any IDerived & AnyObject) {}
// expected-error@-1 {{COM existential containing interface 'IDerived' cannot also contain 'AnyObject'}}

func rejectSuperclass(_: any NativeBase & IDerived) {}
// expected-error@-1 {{COM existential containing interface 'IDerived' cannot also contain class constraint 'NativeBase'}}

func rejectCOMInterface(_: any COMInterface) {}
// expected-error@-1 {{'any COMInterface' is invalid because 'COMInterface' describes a COM metatype identity}}

extension IDerived {
  func method() {
  }
}

func rejectExtensionMember(_ value: any IDerived) {
  value.method()
  // expected-error@-1 {{member 'method' cannot be used on value of type 'any IDerived'; consider using a generic constraint instead}}
}

func acceptGenericExtentionMember<Derived: IDerived>(_ value: Derived) {
  value.method()
}
