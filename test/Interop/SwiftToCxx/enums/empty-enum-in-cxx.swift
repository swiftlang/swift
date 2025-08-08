// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h
// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// Top-level empty enums become C++ namespaces that expose static members
// and nested types.
public enum Utils {
    public static func getAnswer() -> Int { 42 }

    public struct Helper {
        public var x: Int
    }
}

// A nested empty enum inside a struct cannot be a namespace (C++ does not
// allow namespaces inside classes). It should become an unavailable stub.
public struct Container {
    public enum Nested {}

    public var x: Int = 1
}

// A nested empty enum inside a class has the same restriction.
public class HolderClass {
    public enum NestedInClass {}
}

// An empty enum as a struct member is allowed (the struct is still exposed;
// the field's C++ accessor is skipped because the type is a namespace).
public enum Padding {
    public init() { fatalError("uninhabited") }
}
public struct WithPadding {
    public let p: Padding
    public let value: Int
    public init() {
        self.value = 99
        self.p = Padding()
    }
}

// An empty enum as a function parameter makes the function uncallable
// (no value of the parameter type can ever exist), so the function is
// emitted as an unavailable comment. An empty enum as a return type, on
// the other hand, simply means the function never returns — the C++
// thunk is emitted with a `void` return type and `SWIFT_NORETURN`.
public func takesEmpty(_ e: Padding) {}
public func returnsEmpty() -> Padding { fatalError() }

// `@frozen` makes no difference — empty is empty, with or without the
// attribute. The C++ binding is the same.
@frozen
public enum FrozenEmpty {
    public static func id() -> Int { 1 }
}

// A static var on an empty enum: must NOT have the C++ `static` keyword
// at namespace scope (that would mean internal linkage).
public enum WithStaticVar {
    public static var answer: Int { 42 }
}

// `extension Empty { ... }` of an empty enum: the extension's static
// members are emitted into the same namespace as the enum.
public enum Extended {}
extension Extended {
    public static func extFunc() -> Int { 7 }
}

// A generic empty enum cannot become a C++ namespace (namespaces have no
// template parameters). It must be emitted as an unavailable stub.
public enum GenericEmpty<T> {
    public static func tag() -> Int { 1 }
}

// Empty enums wrapped in Optional / Array / Tuple in a function signature
// also bail (each via the common ClangRepresentation::unsupported path
// for the inner type).
public func takesOptional(_ x: Padding?) {}
public func takesArray(_ x: [Padding]) {}
public func takesTuple(_ x: (Int, Padding)) {}

// Top-level type declarations are emitted in alphabetic order, with
// classes/structs interleaved with namespaces.
// CHECK: namespace Extended {
// CHECK:   SWIFT_INLINE_THUNK swift::Int extFunc() SWIFT_SYMBOL(
// CHECK: } // namespace Extended

// CHECK: namespace FrozenEmpty {
// CHECK:   SWIFT_INLINE_THUNK swift::Int id() SWIFT_SYMBOL(
// CHECK: } // namespace FrozenEmpty

// CHECK: namespace Padding {
// CHECK: } // namespace Padding

// CHECK: namespace Utils {
// CHECK:   using Helper=__UtilsNested::Helper;
// CHECK:   SWIFT_INLINE_THUNK swift::Int getAnswer() SWIFT_SYMBOL(
// CHECK: } // namespace Utils

// CHECK: class SWIFT_SYMBOL({{.*}}) WithPadding final {
// CHECK:   SWIFT_INLINE_THUNK swift::Int getValue()

// `static` must NOT appear here — at namespace scope it would mean internal
// linkage. The accessor is a normal inline function in the namespace.
// CHECK: namespace WithStaticVar {
// CHECK-NOT: static
// CHECK:   SWIFT_INLINE_THUNK swift::Int getAnswer() SWIFT_SYMBOL(
// CHECK: } // namespace WithStaticVar

// CHECK: SWIFT_INLINE_THUNK void returnsEmpty() {{.*}}SWIFT_NORETURN

// Generic empty enum becomes an unavailable stub.
// CHECK: class GenericEmpty { } SWIFT_UNAVAILABLE_MSG(

// CHECK: class Nested { } SWIFT_UNAVAILABLE_MSG("empty enum cannot be represented as a namespace");
// CHECK: class NestedInClass { } SWIFT_UNAVAILABLE_MSG("empty enum cannot be represented as a namespace");

// CHECK: // Unavailable in C++: Swift global function 'takesArray
// CHECK: // Unavailable in C++: Swift global function 'takesEmpty
// CHECK: // Unavailable in C++: Swift global function 'takesOptional
// CHECK: // Unavailable in C++: Swift global function 'takesTuple
