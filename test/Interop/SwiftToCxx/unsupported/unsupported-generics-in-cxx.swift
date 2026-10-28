// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Decls -verify -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/decls.h

// RUN: cat %s | grep -v _expose > %t/clean.swift
// RUN: %target-swift-frontend %t/clean.swift -module-name Decls -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/decls.h
// RUN: %FileCheck %s < %t/decls.h

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

public final class GenericClass<T> {}

public func takesGenericClass(_ c: GenericClass<Int>) {}
public func returnsGenericClass() -> GenericClass<Int> { fatalError() }
public func takesOptionalGenericClass(_ c: GenericClass<Int>?) {}
public func takesKeyPath(_ k: KeyPath<Int, Int>) {}

public struct HasGenericClassProperty {
  public let keyPath: KeyPath<Int, Int>
  public var c: GenericClass<Int>
}

public class GenericBase<T> {
  public class func staticMethod() {}
  public func method() {}
}

public class DerivedFromGeneric: GenericBase<Int> {
  public func derivedMethod() {}
}

public class DerivedFromDerivedFromGeneric: DerivedFromGeneric {}

public class OuterGeneric<T> {
  public final class NestedClass {}
}

public func takesNestedInGenericClass(_ c: OuterGeneric<Int>.NestedClass) {}
public func returnsNestedInGenericClass() -> OuterGeneric<Int>.NestedClass { fatalError() }

// CHECK: supported

// Struct1<UInt32> can't be represented in C++, so index1 is not printed.
// CHECK: class SWIFT_SYMBOL("s:5Decls6Class1C") Class1 : public swift::_impl::RefCountedClass {
// CHECK-NEXT: public:
// CHECK-NEXT:   using RefCountedClass::RefCountedClass;
// CHECK-NEXT:   using RefCountedClass::operator=;
// CHECK-NEXT: protected:

// The stored properties of generic class types are not printed.
// CHECK: class SWIFT_SYMBOL("s:5Decls23HasGenericClassPropertyV") HasGenericClassProperty final {
// CHECK-NOT: getKeyPath
// CHECK-NOT: getC
// CHECK: private:

// CHECK: namespace Decls SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Decls") {
// CHECK: namespace Decls SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Decls") {
// CHECK: SWIFT_INLINE_THUNK void supportedFunc(const T_0_0& x) noexcept SWIFT_SYMBOL("s:5Decls13supportedFuncyyxlF") {

// CHECK: class DerivedFromDerivedFromGeneric { } SWIFT_UNAVAILABLE_MSG("class 'DerivedFromDerivedFromGeneric' inherits from a generic class and can not yet be exposed to C++");
// CHECK-EMPTY:
// CHECK-NEXT: class DerivedFromGeneric { } SWIFT_UNAVAILABLE_MSG("class 'DerivedFromGeneric' inherits from a generic class and can not yet be exposed to C++");
// CHECK-EMPTY:
// CHECK-NEXT: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class GenericBase { } SWIFT_UNAVAILABLE_MSG("generic generic class 'GenericBase' can not yet be exposed to C++");

// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class GenericClass { } SWIFT_UNAVAILABLE_MSG("generic generic class 'GenericClass' can not yet be exposed to C++");

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

// CHECK: // Unavailable in C++: Swift global function 'returnsGenericClass()'. Return type 'GenericClass<Int>' is not representable in C++.
// CHECK: // Unavailable in C++: Swift global function 'returnsNestedInGenericClass()'. Return type 'OuterGeneric<Int>.NestedClass' is not representable in C++.
// CHECK: // Unavailable in C++: Swift global function 'takesGenericClass(_:)'. Parameter 'c' of type 'GenericClass<Int>' is not representable in C++.
// CHECK: // Unavailable in C++: Swift global function 'takesKeyPath(_:)'. Parameter 'k' of type 'KeyPath<Int, Int>' is not representable in C++.
// CHECK: // Unavailable in C++: Swift global function 'takesNestedInGenericClass(_:)'. Parameter 'c' of type 'OuterGeneric<Int>.NestedClass' is not representable in C++.
// CHECK: // Unavailable in C++: Swift global function 'takesOptionalGenericClass(_:)'. Parameter 'c' of type 'GenericClass<Int>?' is not representable in C++.

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
