// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=microsoft %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -I %t

import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IBase {}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IDerived: IBase {}

@com(interface: "20000000-0000-0000-0000-000000000001")
protocol IIndependent {}

struct ValueImplementation: IDerived {}
// expected-error@-1 {{non-class type 'ValueImplementation' cannot conform to COM interface 'IDerived'}}

enum EnumImplementation: IIndependent {}
// expected-error@-1 {{non-class type 'EnumImplementation' cannot conform to COM interface 'IIndependent'}}

// expected-error@+1 {{actor 'ActorImplementation' cannot provide a COM implementation}}
actor ActorImplementation: IDerived {}

// Multiple independent interfaces on one implementation are valid.
class MultipleImplementation: IDerived, IIndependent {}

func requiresCompilerManagedInterfaces<
    T: IUnknown & ISwiftObject>(_ value: T) {}

func inferredCompilerManagedInterfaces(
    _ value: MultipleImplementation) {
  requiresCompilerManagedInterfaces(value)
}

// A generic implementation is valid when it has no activation identity.
class GenericImplementation<T>: IDerived {}

@com
class ExplicitGenericImplementation<T>: IDerived {}

@com(implementation: "30000000-0000-0000-0000-000000000001")
class ActivatableGenericImplementation<T>: IDerived {}
// expected-error@-1 {{generic class 'ActivatableGenericImplementation' cannot declare a COM implementation identifier}}

class ConditionalImplementation<T> {}

extension ConditionalImplementation: IIndependent where T: Equatable {}
// expected-error@-1 {{conditional conformance of 'ConditionalImplementation<T>' to COM interface 'IIndependent' is not supported}}
