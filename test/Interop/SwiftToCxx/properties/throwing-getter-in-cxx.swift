// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Properties -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/properties.h
// RUN: %FileCheck %s < %t/properties.h

// RUN: %check-interop-cxx-header-in-clang(%t/properties.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// The header must also compile for consumers that do not opt into the
// experimental Swift error handling support (the throwing bindings are
// guarded out in that case).
// RUN: %check-interop-cxx-header-in-clang(%t/properties.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-function)
// RUN: %check-interop-cxx-header-in-clang(%t/properties.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR -Wno-unused-function)

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

public enum PropError: Error {
    case failure
}

public struct ThrowingProps {
    public var shouldThrow: Bool

    public init(shouldThrow: Bool) {
        self.shouldThrow = shouldThrow
    }

    public var computed: Int {
        get throws {
            print("passThrowingGetter")
            if shouldThrow { throw PropError.failure }
            return 21
        }
    }

    public subscript(index: Int) -> Int {
        get throws {
            print("passThrowingSubscript")
            if shouldThrow { throw PropError.failure }
            return index * 2
        }
    }
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> getComputed() const
// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> operator [](swift::Int index) const

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> ThrowingProps::getComputed() const {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: auto returnValue = Properties::_impl::$s10Properties13ThrowingPropsV8computedSivg(Properties::_impl::swift_interop_passDirect_Properties_bool_0_1(_getOpaquePointer()), _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK-NEXT: }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> ThrowingProps::operator [](swift::Int index) const SWIFT_SYMBOL("s:10Properties13ThrowingPropsVyS2icig") {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: auto returnValue = Properties::_impl::$s10Properties13ThrowingPropsVyS2icig(index, Properties::_impl::swift_interop_passDirect_Properties_bool_0_1(_getOpaquePointer()), _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK-NEXT: }
