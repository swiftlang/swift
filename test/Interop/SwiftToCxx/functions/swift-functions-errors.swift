// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr-or-stdlib -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// The header must also compile for consumers that do not opt into the
// experimental Swift error handling support, and for consumers that
// explicitly hide it: the throwing bindings are guarded out in both cases.
// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-function)
// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR -Wno-unused-function)

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

// NOTE: The generated header emits the function thunks in alphabetical
// order, so the CHECK blocks below are ordered by function name, not by
// source order.

// CHECK-LABEL: namespace Functions SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Functions") {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions18emptyThrowFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // emptyThrowFunction()
// CHECK: SWIFT_EXTERN void $s9Functions20genericThrowFunctionyyxKlF(const void * _Nonnull x, void * _Nonnull , SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // genericThrowFunction(_:)
// CHECK: SWIFT_EXTERN void $s9Functions30genericThrowFunctionWithReturnyxx_SbtKlF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, bool shouldThrow, void * _Nonnull , SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // genericThrowFunctionWithReturn(_:_:)
// CHECK: SWIFT_EXTERN void $s9Functions18testDestroyedErroryyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // testDestroyedError()
// CHECK: SWIFT_EXTERN void $s9Functions13throwFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunction()
// CHECK: SWIFT_EXTERN struct swift_interop_returnStub_Functions_uint64_t_0_8 $s9Functions35throwFunctionWithDirectStructReturnyAA11SmallResultVSbKF(bool shouldThrow, SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithDirectStructReturn(_:)
// CHECK: SWIFT_EXTERN void $s9Functions37throwFunctionWithIndirectStructReturnyAA11LargeResultVSbKF(SWIFT_INDIRECT_RESULT void * _Nonnull, bool shouldThrow, SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithIndirectStructReturn(_:)
// CHECK: SWIFT_EXTERN void $s9Functions28throwFunctionWithNeverReturns0E0OyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithNeverReturn()
// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions31throwFunctionWithPossibleReturnyS2iKF(ptrdiff_t a, SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithPossibleReturn(_:)
// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions23throwFunctionWithReturnSiyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithReturn()
// CHECK: SWIFT_EXTERN struct swift_interop_returnStub_Functions_uint64_t_0_8_void_ptr_8_16 $s9Functions29throwFunctionWithStringReturnySSSbKF(bool shouldThrow, SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithStringReturn(_:)


// CHECK: }

@_expose(Cxx)
public enum NaiveErrors : Error {
    case returnError
    case throwError

    public func getMessage() {
        print(self)
    }
}

class TestDestroyed {
  deinit {
    print("Test destroyed")
  }
}

@_expose(Cxx)
public struct DestroyedError : Error {
  let t = TestDestroyed()
}

@_expose(Cxx)
public struct SmallResult {
    public let value: Int
    public init(_ value: Int) { self.value = value }

    public func doubled(_ shouldThrow: Bool) throws -> SmallResult {
        print("passSmallResultDoubled")
        if shouldThrow { throw NaiveErrors.throwError }
        return SmallResult(value * 2)
    }
}

@_expose(Cxx)
public struct DescriptiveError: Error, CustomStringConvertible {
    public let code: Int
    public init(code: Int) { self.code = code }
    public var description: String { "custom error description" }
}

@_expose(Cxx)
public func throwCustomDescriptionError() throws {
    print("passThrowCustomDescriptionError")
    throw DescriptiveError(code: 7)
}

@_expose(Cxx)
public struct LargeResult {
    public let a: Int
    public let b: Int
    public let c: Int
    public let d: Int
    public let e: Int
    public init() {
        a = 1
        b = 2
        c = 3
        d = 4
        e = 5
    }
}

@_expose(Cxx)
public func emptyThrowFunction() throws { print("passEmptyThrowFunction") }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> emptyThrowFunction() SWIFT_SYMBOL("s:9Functions18emptyThrowFunctionyyKF") {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: _impl::$s9Functions18emptyThrowFunctionyyKF(_ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: #ifndef __cpp_exceptions
// CHECK-NEXT: return swift::Expected<void>();
// CHECK-NEXT: #endif
// CHECK-NEXT: }
// CHECK-NEXT: #endif // defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)

@_expose(Cxx)
public func genericThrowFunction<T>(_ x: T) throws {
    print("passGenericThrowFunction")
    throw NaiveErrors.throwError
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> genericThrowFunction(const T_0_0& x) SWIFT_SYMBOL("s:9Functions20genericThrowFunctionyyxKlF") {
// CHECK: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: _impl::$s9Functions20genericThrowFunctionyyxKlF(swift::_impl::getOpaquePointer(x), swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: #ifndef __cpp_exceptions
// CHECK-NEXT: return swift::Expected<void>();
// CHECK-NEXT: #endif
// CHECK-NEXT: }

@_expose(Cxx)
public func genericThrowFunctionWithReturn<T>(_ x: T, _ shouldThrow: Bool) throws -> T {
    print("passGenericThrowFunctionWithReturn")
    if shouldThrow { throw NaiveErrors.throwError }
    return x
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<T_0_0> genericThrowFunctionWithReturn(const T_0_0& x, bool shouldThrow) SWIFT_SYMBOL("s:9Functions30genericThrowFunctionWithReturnyxx_SbtKlF") SWIFT_WARN_UNUSED_RESULT {
// CHECK: if constexpr (std::is_base_of<::swift::_impl::RefCountedClass, T_0_0>::value) {
// CHECK-NEXT: void *returnValue;
// CHECK-NEXT: _impl::$s9Functions30genericThrowFunctionWithReturnyxx_SbtKlF(reinterpret_cast<void *>(&returnValue), swift::_impl::getOpaquePointer(x), shouldThrow, swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<T_0_0>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return ::swift::_impl::implClassFor<T_0_0>::type::makeRetained(returnValue);
// CHECK-NEXT: } else if constexpr (::swift::_impl::isValueType<T_0_0>) {
// CHECK-NEXT: void *returnMetadata = swift::TypeMetadataTrait<T_0_0>::getTypeMetadata();
// CHECK-NEXT: auto *returnVWTableAddr = reinterpret_cast<::swift::_impl::ValueWitnessTable **>(returnMetadata) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT: auto *returnVWTable = reinterpret_cast<::swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*returnVWTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(returnVWTableAddr, {{.*}})));
// CHECK-NEXT: #else
// CHECK-NEXT: auto *returnVWTable = *returnVWTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT: ::swift::_impl::OpaqueStorage returnStorage(returnVWTable->size, returnVWTable->getAlignment());
// CHECK-NEXT: _impl::$s9Functions30genericThrowFunctionWithReturnyxx_SbtKlF(returnStorage.getOpaquePointer(), swift::_impl::getOpaquePointer(x), shouldThrow, swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<T_0_0>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return ::swift::_impl::implClassFor<T_0_0>::type::returnNewValue([&](void * _Nonnull returnValue) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: ::swift::_impl::implClassFor<T_0_0>::type::initializeWithTake(reinterpret_cast<char * _Nonnull>(returnValue), returnStorage.getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: } else if constexpr (::swift::_impl::isSwiftBridgedCxxRecord<T_0_0>) {
// CHECK: } else {
// CHECK-NEXT: T_0_0 returnValue;
// CHECK-NEXT: _impl::$s9Functions30genericThrowFunctionWithReturnyxx_SbtKlF(reinterpret_cast<void *>(&returnValue), swift::_impl::getOpaquePointer(x), shouldThrow, swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<T_0_0>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return returnValue;
// CHECK-NEXT: }

@_expose(Cxx)
public func testDestroyedError() throws { throw DestroyedError() }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> testDestroyedError() SWIFT_SYMBOL("s:9Functions18testDestroyedErroryyKF") {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18testDestroyedErroryyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: }

@_expose(Cxx)
public func throwFunction() throws {
    print("passThrowFunction")
    throw NaiveErrors.throwError
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> throwFunction() SWIFT_SYMBOL("s:9Functions13throwFunctionyyKF") {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions13throwFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: }

@_expose(Cxx)
public func throwFunctionWithDirectStructReturn(_ shouldThrow: Bool) throws -> SmallResult {
    print("passThrowFunctionWithDirectStructReturn")
    if shouldThrow { throw NaiveErrors.throwError }
    return SmallResult(42)
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<SmallResult> throwFunctionWithDirectStructReturn(bool shouldThrow) SWIFT_SYMBOL("s:9Functions35throwFunctionWithDirectStructReturnyAA11SmallResultVSbKF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: auto returnValue = Functions::_impl::$s9Functions35throwFunctionWithDirectStructReturnyAA11SmallResultVSbKF(shouldThrow, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<SmallResult>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return Functions::_impl::_impl_SmallResult::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: Functions::_impl::swift_interop_returnDirect_Functions_uint64_t_0_8(result, returnValue);
// CHECK-NEXT: });
// CHECK-NEXT: }

@_expose(Cxx)
public func throwFunctionWithIndirectStructReturn(_ shouldThrow: Bool) throws -> LargeResult {
    print("passThrowFunctionWithIndirectStructReturn")
    if shouldThrow { throw NaiveErrors.throwError }
    return LargeResult()
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<LargeResult> throwFunctionWithIndirectStructReturn(bool shouldThrow) SWIFT_SYMBOL("s:9Functions37throwFunctionWithIndirectStructReturnyAA11LargeResultVSbKF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: void *returnMetadata = swift::TypeMetadataTrait<LargeResult>::getTypeMetadata();
// CHECK-NEXT: auto *returnVWTableAddr = reinterpret_cast<::swift::_impl::ValueWitnessTable **>(returnMetadata) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT: auto *returnVWTable = reinterpret_cast<::swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*returnVWTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(returnVWTableAddr, {{.*}})));
// CHECK-NEXT: #else
// CHECK-NEXT: auto *returnVWTable = *returnVWTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT: ::swift::_impl::OpaqueStorage returnStorage(returnVWTable->size, returnVWTable->getAlignment());
// CHECK-NEXT: Functions::_impl::$s9Functions37throwFunctionWithIndirectStructReturnyAA11LargeResultVSbKF(returnStorage.getOpaquePointer(), shouldThrow, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<LargeResult>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return Functions::_impl::_impl_LargeResult::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: Functions::_impl::_impl_LargeResult::initializeWithTake(result, returnStorage.getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: }

@_expose(Cxx)
public func throwFunctionWithNeverReturn() throws -> Never {
    print("passThrowFunctionWithNeverReturn")
    throw NaiveErrors.returnError
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> throwFunctionWithNeverReturn() SWIFT_SYMBOL("s:9Functions28throwFunctionWithNeverReturns0E0OyKF") SWIFT_NORETURN_EXCEPT_ERRORS {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: _impl::$s9Functions28throwFunctionWithNeverReturns0E0OyKF(_ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: abort();
// CHECK-NEXT: }

@_expose(Cxx)
public func throwFunctionWithPossibleReturn(_ a: Int) throws -> Int {
    print("passThrowFunctionWithPossibleReturn")
    if (a == 0) {
        throw NaiveErrors.returnError
    }
    return 0
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> throwFunctionWithPossibleReturn(swift::Int a) SWIFT_SYMBOL("s:9Functions31throwFunctionWithPossibleReturnyS2iKF") SWIFT_WARN_UNUSED_RESULT {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: auto returnValue = Functions::_impl::$s9Functions31throwFunctionWithPossibleReturnyS2iKF(a, _ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK: }

@_expose(Cxx)
public func throwFunctionWithReturn() throws -> Int {
    print("passThrowFunctionWithReturn")
    throw NaiveErrors.returnError
    return 0
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> throwFunctionWithReturn() SWIFT_SYMBOL("s:9Functions23throwFunctionWithReturnSiyKF") SWIFT_WARN_UNUSED_RESULT {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: auto returnValue = Functions::_impl::$s9Functions23throwFunctionWithReturnSiyKF(_ctx, &opaqueError);
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK: }

@_expose(Cxx)
public func throwFunctionWithStringReturn(_ shouldThrow: Bool) throws -> String {
    print("passThrowFunctionWithStringReturn")
    if shouldThrow { throw NaiveErrors.throwError }
    return "Hello from Swift"
}

// Each throwing thunk is wrapped in the guard for the experimental Swift
// error handling support.
// CHECK: #if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) && !defined(SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR)
// CHECK-NEXT: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::String> throwFunctionWithStringReturn(bool shouldThrow) SWIFT_SYMBOL("s:9Functions29throwFunctionWithStringReturnySSSbKF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: auto returnValue = Functions::_impl::$s9Functions29throwFunctionWithStringReturnySSSbKF(shouldThrow, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<swift::String>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return swift::_impl::_impl_String::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: Functions::_impl::swift_interop_returnDirect_Functions_uint64_t_0_8_void_ptr_8_16(result, returnValue);
// CHECK-NEXT: });
// CHECK-NEXT: }
