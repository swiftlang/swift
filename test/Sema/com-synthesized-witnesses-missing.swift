// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -D MISSING_ISWIFTOBJECT_DEFAULTS %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -enable-experimental-com-interop -com-interop-model=microsoft -I %t
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -enable-experimental-com-interop -com-interop-model=corefoundation -I %t

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget {}

// These classes are deliberately unused. Checking their declarations must
// diagnose the missing witnesses without an explicit conformance lookup.
@com
class Explicit {} // expected-error {{type 'Explicit' does not conform to protocol 'ISwiftObject'}}
// expected-note@-1 {{add stubs for conformance}}

class Inferred: IWidget {} // expected-error {{type 'Inferred' does not conform to protocol 'ISwiftObject'}}
// expected-note@-1 {{add stubs for conformance}}

class Generic<T>: IWidget {} // expected-error {{type 'Generic<T>' does not conform to protocol 'ISwiftObject'}}
// expected-note@-1 {{add stubs for conformance}}

class Extended {} // expected-error {{type 'Extended' does not conform to protocol 'ISwiftObject'}}
// expected-note@-1 {{add stubs for conformance}}
extension Extended: IWidget {}

class Partial: IWidget { // expected-error {{type 'Partial' does not conform to protocol 'ISwiftObject'}}
  // expected-note@-1 {{add stubs for conformance}}
  var object: UnsafeMutableRawPointer { fatalError() }
}

class MemberWitnesses: IWidget {
  var object: UnsafeMutableRawPointer { fatalError() }
  var metadata: UnsafeRawPointer { fatalError() }
}

class Derived: MemberWitnesses {}

class Ordinary {}

// Enabling COM interop must not give an ordinary class Swift COM identity.
func requiresIdentity<T: ISwiftObject>(_: T.Type) {}
// expected-note@-1 {{where 'T' = 'Ordinary'}}
func rejectOrdinaryClass() {
  requiresIdentity(Ordinary.self)
  // expected-error@-1 {{global function 'requiresIdentity' requires that 'Ordinary' conform to 'ISwiftObject'}}
}

// IWidget does not imply AnyObject, but Swift implementations must be classes.
// Reject value types without also synthesizing an ISwiftObject conformance.
struct ValueImplementation: IWidget {}
// expected-error@-1 {{non-class type 'ValueImplementation' cannot conform to COM interface 'IWidget'}}

enum EnumImplementation: IWidget {}
// expected-error@-1 {{non-class type 'EnumImplementation' cannot conform to COM interface 'IWidget'}}
