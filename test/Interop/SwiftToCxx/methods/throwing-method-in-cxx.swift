// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Methods -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/methods.h
// RUN: %FileCheck %s < %t/methods.h

// RUN: %check-interop-cxx-header-in-clang(%t/methods.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// The header must also compile for consumers that do not opt into the
// experimental Swift error handling support (the throwing bindings are
// guarded out in that case).
// RUN: %check-interop-cxx-header-in-clang(%t/methods.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-function)
// RUN: %check-interop-cxx-header-in-clang(%t/methods.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR -Wno-unused-function)

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

public enum MethodError: Error {
    case failure
}

public struct LargeMethodResult {
    public let a: Int
    public let b: Int
    public let c: Int
    public let d: Int
    public let e: Int
}

public struct ThrowingStruct {
    var x: Int

    public init(_ x: Int) { self.x = x }

    public func throwingMethod(_ shouldThrow: Bool) throws -> Int {
        print("passThrowingMethod")
        if shouldThrow { throw MethodError.failure }
        return x
    }

    public func throwingVoidMethod(_ shouldThrow: Bool) throws {
        print("passThrowingVoidMethod")
        if shouldThrow { throw MethodError.failure }
    }

    public mutating func throwingMutatingMethod(_ shouldThrow: Bool) throws {
        print("passThrowingMutatingMethod")
        if shouldThrow { throw MethodError.failure }
        x += 1
    }

    public static func throwingStaticMethod(_ shouldThrow: Bool) throws -> Int {
        print("passThrowingStaticMethod")
        if shouldThrow { throw MethodError.failure }
        return 99
    }

    public func throwingStructReturn(_ shouldThrow: Bool) throws -> ThrowingStruct {
        print("passThrowingStructReturn")
        if shouldThrow { throw MethodError.failure }
        return ThrowingStruct(x + 100)
    }

    public func throwingLargeStructReturn(_ shouldThrow: Bool) throws -> LargeMethodResult {
        print("passThrowingLargeStructReturn")
        if shouldThrow { throw MethodError.failure }
        return LargeMethodResult(a: 1, b: 2, c: 3, d: 4, e: 5)
    }
}

public final class ThrowingClass {
    var counter: Int

    public init() { counter = 0 }

    public func throwingClassMethod(_ shouldThrow: Bool) throws -> Int {
        print("passThrowingClassMethod")
        if shouldThrow { throw MethodError.failure }
        return 42
    }

    public func throwingReturnsClass(_ shouldThrow: Bool) throws -> ThrowingClass {
        print("passThrowingReturnsClass")
        if shouldThrow { throw MethodError.failure }
        return self
    }
}

// NOTE: The generated header emits the thunks in alphabetical order by type,
// so the CHECK blocks below follow the emission order.

// CHECK: SWIFT_EXTERN ptrdiff_t $s7Methods14ThrowingStructV14throwingMethodySiSbKF(bool shouldThrow, struct swift_interop_passStub_Methods_uint64_t_0_8 _self, SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwingMethod(_:)

// A method on a class: `self` is the context, so no placeholder context
// variable is emitted.
// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> ThrowingClass::throwingClassMethod(bool shouldThrow) {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: auto returnValue = Methods::_impl::$s7Methods13ThrowingClassC08throwingC6MethodySiSbKF(shouldThrow, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this), &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK-NEXT: }

// A method returning a class instance: the opaque pointer is stashed and
// checked for an error before it is wrapped.
// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<ThrowingClass> ThrowingClass::throwingReturnsClass(bool shouldThrow) {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void *returnValue = Methods::_impl::$s7Methods13ThrowingClassC015throwingReturnsC0yACSbKF(shouldThrow, ::swift::_impl::_impl_RefCountedClass::getOpaquePointer(*this), &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<ThrowingClass>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return _impl::_impl_ThrowingClass::makeRetained(returnValue);
// CHECK-NEXT: }

// A method on a struct: small `self` is passed directly and the lowered
// signature carries a synthesized context parameter.
// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> ThrowingStruct::throwingMethod(bool shouldThrow) const {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: auto returnValue = Methods::_impl::$s7Methods14ThrowingStructV14throwingMethodySiSbKF(shouldThrow, Methods::_impl::swift_interop_passDirect_Methods_uint64_t_0_8(_getOpaquePointer()), _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK-NEXT: }
