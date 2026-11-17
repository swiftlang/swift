// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Existentials -verify -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/existentials.h
// RUN: %FileCheck %s < %t/existentials.h

// RUN: %check-interop-cxx-header-in-clang(%t/existentials.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -std=c++17)

public struct Foo {
    var x: CInt
    var y: CInt
};

// Members that can't be represented in C++ get a comment in their type's body.
// Non-public, @_expose(!Cxx) and synthesized members (for Hashable and Codable)
// don't.

public final class ClassWithExistentials {
    public init() {}
    public func describe(_ error: any Error) -> String { "\(error)" }
    public func number() -> Double { 1 }
    private func privateDescribe(_ error: any Error) {}
    func internalDescribe(_ error: any Error) {}
    @_expose(!Cxx)
    public func notExposedDescribe(_ error: any Error) {}
}

// CHECK: class SWIFT_SYMBOL("s:12Existentials09ClassWithA0C") ClassWithExistentials final : public swift::_impl::RefCountedClass {
// CHECK:        static SWIFT_INLINE_THUNK ClassWithExistentials init() noexcept SWIFT_SYMBOL("s:12Existentials09ClassWithA0CACycfc");
// CHECK-NEXT:   // Unavailable in C++: Swift instance method 'describe(_:)'. Parameter 'error' of type 'any Error' is not representable in C++.
// CHECK-NEXT:   SWIFT_INLINE_THUNK double number() noexcept SWIFT_SYMBOL("s:12Existentials09ClassWithA0C6numberSdyF");
// CHECK-NEXT: protected:

public enum EnumWithExistentials {
    case a, b
    public func describe(_ error: any Error) -> String { "\(error)" }
}

// CHECK: class SWIFT_SYMBOL("s:12Existentials08EnumWithA0O") EnumWithExistentials final {
// CHECK:        SWIFT_INLINE_THUNK operator cases() const {
// CHECK:        // Unavailable in C++: Swift instance method 'describe(_:)'. Parameter 'error' of type 'any Error' is not representable in C++.
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int getHashValue() const noexcept SWIFT_SYMBOL("s:12Existentials08EnumWithA0O9hashValueSivp");
// CHECK-NEXT: private:

public struct StructWithExistentials: Hashable, Codable {
    public var x: CInt
    public init(x: CInt) { self.x = x }
    public init(error: any Error) { x = 0 }
    public var error: (any Error)? {
        get { nil }
        set {}
    }
    public subscript(error: any Error) -> CInt { x }
    public func describe(_ error: any Error) -> String { "\(error)" }
}

// CHECK: class SWIFT_SYMBOL("s:12Existentials010StructWithA0V") StructWithExistentials final {
// CHECK:        SWIFT_INLINE_THUNK int getX() const noexcept SWIFT_SYMBOL("s:12Existentials010StructWithA0V1xs5Int32Vvp");
// CHECK-NEXT:   SWIFT_INLINE_THUNK void setX(int value) noexcept SWIFT_SYMBOL("s:12Existentials010StructWithA0V1xs5Int32Vvp");
// CHECK-NEXT:   static SWIFT_INLINE_THUNK StructWithExistentials init(int x) noexcept SWIFT_SYMBOL("s:12Existentials010StructWithA0V1xACs5Int32V_tcfc");
// CHECK-NEXT:   // Unavailable in C++: Swift initializer 'init(error:)'. Parameter 'error' of type 'any Error' is not representable in C++.
// CHECK-NEXT:   // Unavailable in C++: Swift property 'error'. Return type '(any Error)?' is not representable in C++.
// CHECK-NEXT:   // Unavailable in C++: Swift subscript 'subscript(_:)'. Parameter 'error' of type 'any Error' is not representable in C++.
// CHECK-NEXT:   // Unavailable in C++: Swift instance method 'describe(_:)'. Parameter 'error' of type 'any Error' is not representable in C++.
// CHECK-NEXT:   SWIFT_INLINE_THUNK swift::Int getHashValue() const noexcept SWIFT_SYMBOL("s:12Existentials010StructWithA0V9hashValueSivp");
// CHECK-NEXT: private:

public func useExistential(_ x: KeyPath<Foo, CInt> & Sendable) {
}

// CHECK: Unavailable in C++: Swift global function 'useExistential(_:)'. Parameter 'x' of type {{.*}} is not representable in C++.
