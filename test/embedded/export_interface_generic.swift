// RUN: %target-typecheck-verify-swift -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

// In Embedded Swift, @export(interface) cannot be used on generic types or on
// extensions that introduce generics, because IRGen emits a unique strong
// definition of the type metadata / conformance witness tables, which is
// incompatible with per-specialization emission.

public protocol P {}

// Non-generic types are allowed.
@export(interface)
public struct NonGenericStruct {}

@export(interface)
public class NonGenericClass {}

@export(interface)
public enum NonGenericEnum {}

// Generic types are rejected.
@export(interface) // expected-error {{'@export(interface)' cannot be applied to a generic struct 'GenericStruct' in Embedded Swift}}
public struct GenericStruct<T> {}

@export(interface) // expected-error {{'@export(interface)' cannot be applied to a generic class 'GenericClass' in Embedded Swift}}
public class GenericClass<T> {}

@export(interface) // expected-error {{'@export(interface)' cannot be applied to a generic enum 'GenericEnum' in Embedded Swift}}
public enum GenericEnum<T> {
  case a(T)
}

// An extension of a non-generic type is allowed.
public struct PlainStruct {}

@export(interface)
extension PlainStruct: P {}

// An extension of a generic type is rejected (the extension inherits the
// generic context from the extended type).
public struct GenericHolder<T> {}

@export(interface) // expected-error {{'@export(interface)' cannot be applied to a extension of generic struct 'GenericHolder' in Embedded Swift}}
extension GenericHolder: P {}

// @export(implementation) does not have this restriction.
@export(implementation)
public struct GenericImplStruct<T> {}
