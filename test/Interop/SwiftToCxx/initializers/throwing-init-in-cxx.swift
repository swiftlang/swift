// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Init -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/inits.h
// RUN: %FileCheck %s < %t/inits.h

// RUN: %check-interop-cxx-header-in-clang(%t/inits.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// The header must also compile for consumers that do not opt into the
// experimental Swift error handling support (the throwing bindings are
// guarded out in that case).
// RUN: %check-interop-cxx-header-in-clang(%t/inits.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-function)
// RUN: %check-interop-cxx-header-in-clang(%t/inits.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR -Wno-unused-function)

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

public enum InitError: Error {
    case failure
}

final class Canary {
    deinit { print("Canary destroyed") }
}

public struct StructWithThrowingInit {
    public let value: Int

    public init(checking value: Int) throws {
        print("passStructThrowingInit")
        if value < 0 { throw InitError.failure }
        self.value = value
    }
}

public struct RefHolderWithThrowingInit {
    var canary: Canary

    public init(shouldThrow: Bool) throws {
        print("passRefHolderThrowingInit")
        if shouldThrow { throw InitError.failure }
        self.canary = Canary()
    }
}

public struct LargeStructWithThrowingInit {
    public let a: Int
    public let b: Int
    public let c: Int
    public let d: Int
    public let e: Int

    public init(checking value: Int) throws {
        print("passLargeStructThrowingInit")
        if value < 0 { throw InitError.failure }
        a = value
        b = value + 1
        c = value + 2
        d = value + 3
        e = value + 4
    }
}

public final class ClassWithThrowingInit {
    public let value: Int

    public init(checking value: Int) throws {
        print("passClassThrowingInit")
        if value < 0 { throw InitError.failure }
        self.value = value
    }

    deinit {
        print("ClassWithThrowingInit destroyed")
    }
}

// NOTE: The generated header emits the thunks in alphabetical order, so the
// CHECK blocks below are ordered by type name, not by source order.

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<ClassWithThrowingInit> ClassWithThrowingInit::init(swift::Int value) {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void *returnValue = Init::_impl::$s4Init017ClassWithThrowingA0C8checkingACSi_tKcfC(value, swift::TypeMetadataTrait<ClassWithThrowingInit>::getTypeMetadata(), &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<ClassWithThrowingInit>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return _impl::_impl_ClassWithThrowingInit::makeRetained(returnValue);
// CHECK-NEXT: }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<LargeStructWithThrowingInit> LargeStructWithThrowingInit::init(swift::Int value) {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: void *returnMetadata = swift::TypeMetadataTrait<LargeStructWithThrowingInit>::getTypeMetadata();
// CHECK-NEXT: auto *returnVWTableAddr = reinterpret_cast<::swift::_impl::ValueWitnessTable **>(returnMetadata) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT: auto *returnVWTable = reinterpret_cast<::swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*returnVWTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(returnVWTableAddr, {{.*}})));
// CHECK-NEXT: #else
// CHECK-NEXT: auto *returnVWTable = *returnVWTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT: ::swift::_impl::OpaqueStorage returnStorage(returnVWTable->size, returnVWTable->getAlignment());
// CHECK-NEXT: Init::_impl::$s4Init023LargeStructWithThrowingA0V8checkingACSi_tKcfC(returnStorage.getOpaquePointer(), value, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<LargeStructWithThrowingInit>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return Init::_impl::_impl_LargeStructWithThrowingInit::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: Init::_impl::_impl_LargeStructWithThrowingInit::initializeWithTake(result, returnStorage.getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<RefHolderWithThrowingInit> RefHolderWithThrowingInit::init(bool shouldThrow) {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: auto returnValue = Init::_impl::$s4Init021RefHolderWithThrowingA0V11shouldThrowACSb_tKcfC(shouldThrow, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<RefHolderWithThrowingInit>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return Init::_impl::_impl_RefHolderWithThrowingInit::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: Init::_impl::swift_interop_returnDirect_Init_void_ptr_0_8(result, returnValue);
// CHECK-NEXT: });
// CHECK-NEXT: }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<StructWithThrowingInit> StructWithThrowingInit::init(swift::Int value) {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: auto returnValue = Init::_impl::$s4Init018StructWithThrowingA0V8checkingACSi_tKcfC(value, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<StructWithThrowingInit>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return Init::_impl::_impl_StructWithThrowingInit::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: Init::_impl::swift_interop_returnDirect_Init_uint64_t_0_8(result, returnValue);
// CHECK-NEXT: });
// CHECK-NEXT: }
