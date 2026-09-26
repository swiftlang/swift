// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Decls -verify -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/decls.h

// RUN: cat %s | grep -v _expose > %t/clean.swift
// RUN: %target-swift-frontend %t/clean.swift -module-name Decls -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/decls.h
// RUN: %FileCheck %s --implicit-check-not=ContextNested < %t/decls.h

// RUN: %check-interop-cxx-header-in-clang(%t/decls.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public protocol Proto { init() }

@_expose(Cxx)
public func supportedFunc<T>(_ x: T) {}

@_expose(Cxx) // expected-error {{generic requirements for global function 'unsupportedFunc' can not yet be represented in C++}}
public func unsupportedFunc<T: Proto>(_ x: T) {}

@_expose(Cxx) // expected-error {{generic generic class 'unsupportedGenericClass' can not yet be exposed to C++}}
public class unsupportedGenericClass<T> {
    var v: T?

    public init() {
        v = nil
    }
}

@_expose(Cxx) // expected-error {{generic requirements for generic struct 'unsupportedGenericStruct' can not yet be represented in C++}}
public struct unsupportedGenericStruct<T: Proto> {
    var v: T
}

@_expose(Cxx) // expected-error {{generic requirements for generic enum 'unsupportedGenericEnum' can not yet be represented in C++}}
public enum unsupportedGenericEnum<T: Proto> {
    case A
    case B(T)
}

public struct Struct1<IntType: FixedWidthInteger> {
  private var value: IntType

  public init(rawValue: IntType) {
    self.value = rawValue
  }
}

public class Class1 {
  public var index1: Struct1<UInt32> { .init(rawValue: 123) }
}

public struct GlyphIndex<IntType: FixedWidthInteger & UnsignedInteger> {
    private var value: IntType

    init(value: IntType) {
        self.value = value
    }
}

public struct QueryResult<GlyphIndexInt: UnsignedInteger & FixedWidthInteger> {
    public var glyphIDs: [GlyphIndex<GlyphIndexInt>]
}

public func makeQueryResult() -> QueryResult<UInt32> { .init(glyphIDs: []) }

// Requirements of the generic context are validated even for declarations
// without a generic parameter list of their own.
@_expose(Cxx)
public struct ContextuallyGeneric<T> {
    var v: T

    @_expose(Cxx) // expected-error {{generic requirements for initializer 'init()' can not yet be represented in C++}}
    public init() where T: Proto { v = T() }

    @_expose(Cxx) // expected-error {{generic requirements for instance method 'requiresProto()' can not yet be represented in C++}}
    public func requiresProto() where T: Proto {}

    @_expose(Cxx) // expected-error {{generic requirements for static method 'staticRequiresProto()' can not yet be represented in C++}}
    public static func staticRequiresProto() where T: Proto {}

    public subscript(_ x: Int) -> Int where T: Proto { return x }

    // A marker protocol requirement does not need a witness table, so this
    // method is still exposed.
    @_expose(Cxx)
    public func requiresSendable() where T: Sendable {}

    @_expose(Cxx) // expected-error {{class 'ContextNestedClass' nested in a generic context can not yet be represented in C++}}
    public class ContextNestedClass {}

    @_expose(Cxx) // expected-error {{struct 'ContextNestedStruct' nested in a generic context can not yet be represented in C++}}
    public struct ContextNestedStruct {}

    @_expose(Cxx) // expected-error {{enum 'ContextNestedEnum' nested in a generic context can not yet be represented in C++}}
    public enum ContextNestedEnum { case a }

    @_expose(Cxx) // expected-error {{generic struct 'ContextNestedGeneric' nested in a generic context can not yet be represented in C++}}
    public struct ContextNestedGeneric<U> { var u: U }
}

extension ContextuallyGeneric {
    @_expose(Cxx) // expected-error {{struct 'ContextNestedInExtension' nested in a generic context can not yet be represented in C++}}
    public struct ContextNestedInExtension {}
}

extension ContextuallyGeneric where T: Proto {
    @_expose(Cxx) // expected-error {{generic requirements for instance method 'inConstrainedExtension()' can not yet be represented in C++}}
    public func inConstrainedExtension() {}

    @_expose(Cxx) // expected-error {{generic requirements for property 'inConstrainedExtensionVar' can not yet be represented in C++}}
    public var inConstrainedExtensionVar: Int { 0 }
}

// The generated bindings call the direct form of the type metadata accessor,
// which takes at most three generic arguments.
@_expose(Cxx) // expected-error {{generic struct 'FourGenericParams' can not yet be represented in C++ as it has more than 3 generic parameters}}
public struct FourGenericParams<A, B, C, D> {
    var a: A
    var b: B
    var c: C
    var d: D
}

public func takesFourGenericParams(_ x: FourGenericParams<Int, Int, Int, Int>) {}

// CHECK: supported

// CHECK: class SWIFT_SYMBOL("s:5Decls6Class1C") Class1 : public swift::_impl::RefCountedClass {
// CHECK: 'index1' cannot be printed

// CHECK: namespace Decls SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Decls") {
// CHECK: namespace Decls SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Decls") {
// CHECK: class SWIFT_SYMBOL("s:5Decls19ContextuallyGenericV") ContextuallyGeneric final {
// CHECK-NOT: init(
// CHECK-NOT: requiresProto
// CHECK-NOT: operator []
// CHECK: SWIFT_INLINE_THUNK void requiresSendable() const
// CHECK-NOT: inConstrainedExtension
// CHECK: };
// CHECK: SWIFT_INLINE_THUNK void supportedFunc(const T_0_0& x) noexcept SWIFT_SYMBOL("s:5Decls13supportedFuncyyxlF") {
// CHECK: SWIFT_INLINE_THUNK void ContextuallyGeneric<T_0_0>::requiresSendable() const noexcept {

// CHECK: template<class T_0_0, class T_0_1, class T_0_2, class T_0_3>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1> && swift::isUsableInGenericContext<T_0_2> && swift::isUsableInGenericContext<T_0_3>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class FourGenericParams { } SWIFT_UNAVAILABLE_MSG("generic struct 'FourGenericParams' can not yet be represented in C++ as it has more than 3 generic parameters");

// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class GlyphIndex { } SWIFT_UNAVAILABLE_MSG("generic requirements for generic struct 'GlyphIndex' can not yet be represented in C++");

// CHECK: class Proto { } SWIFT_UNAVAILABLE_MSG("protocol 'Proto' can not yet be represented in C++");

// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class QueryResult { } SWIFT_UNAVAILABLE_MSG("generic requirements for generic struct 'QueryResult' can not yet be represented in C++");

// CHECK: class Struct1 { } SWIFT_UNAVAILABLE_MSG("generic requirements for generic struct 'Struct1' can not yet be represented in C++");

// CHECK: // Unavailable in C++: Swift global function 'unsupportedFunc(_:)'.{{.*}}can not yet be represented in C++.

// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class unsupportedGenericClass { } SWIFT_UNAVAILABLE_MSG("generic generic class 'unsupportedGenericClass' can not yet be exposed to C++");
// CHECK-EMPTY:
// CHECK-NEXT: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class unsupportedGenericEnum { } SWIFT_UNAVAILABLE_MSG("generic requirements for generic enum 'unsupportedGenericEnum' can not yet be represented in C++");
// CHECK-EMPTY:
// CHECK-NEXT: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class unsupportedGenericStruct { } SWIFT_UNAVAILABLE_MSG("generic requirements for generic struct 'unsupportedGenericStruct' can not yet be represented in C++");
