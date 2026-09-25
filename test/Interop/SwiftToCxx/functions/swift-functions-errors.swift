// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr-or-stdlib -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// RUN: %target-swift-frontend %s -module-name Functions -enable-experimental-cxx-interop \
// RUN:   -clang-header-expose-decls=has-expose-attr-or-stdlib \
// RUN:   -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX \
// RUN:   -enable-library-evolution -typecheck -verify -emit-clang-header-path %t/functions-resilient.h
// RUN: %FileCheck --check-prefix=RESILIENT %s < %t/functions-resilient.h

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

// CHECK-LABEL: namespace Functions SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Functions") {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions18emptyThrowFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // emptyThrowFunction()
// CHECK: SWIFT_EXTERN void $s9Functions18testDestroyedErroryyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // testDestroyedError()
// CHECK: SWIFT_EXTERN void $s9Functions13throwFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunction()
// CHECK: SWIFT_EXTERN void $s9Functions28throwFunctionWithNeverReturns0E0OyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithNeverReturn()
// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions31throwFunctionWithPossibleReturnyS2iKF(ptrdiff_t a, SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithPossibleReturn(_:)
// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions23throwFunctionWithReturnSiyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithReturn()


// CHECK: }

// CHECK: swift::ThrowingResult<void> never() {{.*}} SWIFT_NORETURN_EXCEPT_ERRORS;

@_expose(Cxx)
public enum NaiveErrors : Error {
    case returnError
    case throwError

    public func getMessage() {
        print(self)
    }
}

@_expose(Cxx)
public func checkedVoid(_ fail: Bool) throws {
  if fail { throw NaiveErrors.throwError }
}

@_expose(Cxx)
public func emptyThrowFunction() throws { print("passEmptyThrowFunction") }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> emptyThrowFunction() SWIFT_SYMBOL("s:9Functions18emptyThrowFunctionyyKF") {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18emptyThrowFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK-NEXT: #ifndef __cpp_exceptions
// CHECK-NEXT: return swift::Expected<void>();
// CHECK-NEXT: #endif
// CHECK-NEXT: }

@_expose(Cxx)
public func genericVoid<T>(_ value: T, _ fail: Bool) throws {
  try checkedVoid(fail)
}

@_expose(Cxx)
public func genericNever<T>(_ value: T) throws -> Never {
  throw NaiveErrors.throwError
}

// Generic Void and Never results must not cast a GenericFunctionType to FunctionType.
// CHECK: swift::ThrowingResult<void> genericNever
// CHECK: abort();
// CHECK: swift::ThrowingResult<void> genericVoid
// CHECK: #ifndef __cpp_exceptions
// CHECK-NEXT: return swift::Expected<void>();

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

// Counts the live instances, to check that no Swift value is leaked or
// destroyed twice when an error is thrown.
final class Canary {
  static var living = 0
  init() { Canary.living += 1 }
  deinit { Canary.living -= 1 }
}

@_expose(Cxx)
public func livingCanaries() -> Int { Canary.living }

@_expose(Cxx)
public struct SmallResult {
  public let value: Int
  public init(_ value: Int, _ fail: Bool) throws {
    try checkedVoid(fail)
    self.value = value
  }
  public func doubled(_ fail: Bool) throws -> SmallResult {
    try SmallResult(value * 2, fail)
  }
  public static func make(_ fail: Bool) throws -> SmallResult {
    try SmallResult(42, fail)
  }
}

@_expose(Cxx)
public struct LargeResult {
  public let a, b, c, d, e: Int
  let canary: Canary
  public init(_ fail: Bool) throws {
    // A failing initializer must clean up the partially initialized value.
    canary = Canary()
    try checkedVoid(fail)
    (a, b, c, d, e) = (1, 2, 3, 4, 5)
  }
}

@_expose(Cxx)
public final class RefResult {
  public let value: Int
  public init(_ fail: Bool) throws {
    try checkedVoid(fail)
    value = 42
  }
  public func small(_ fail: Bool) throws -> SmallResult {
    try SmallResult(value, fail)
  }
}

// The parameters are named like a local of other thunks, to check that the
// locals of these thunks don't clash with them.
@_expose(Cxx)
public func throwingSmall(_ returnValue: Bool) throws -> SmallResult {
  try SmallResult(42, returnValue)
}

@_expose(Cxx)
public func throwingLarge(_ returnValue: Bool) throws -> LargeResult {
  try LargeResult(returnValue)
}

@_expose(Cxx)
public func throwingOptionalLarge(_ returnValue: Bool) throws -> LargeResult? {
  try LargeResult(returnValue)
}

@_expose(Cxx)
public func throwingRef(_ returnValue: Bool) throws -> RefResult {
  try RefResult(returnValue)
}

@_expose(Cxx)
public func throwingString(_ fail: Bool) throws -> String {
  try checkedVoid(fail)
  return "Hello"
}

@_expose(Cxx)
public func throwingGeneric<T>(_ value: T, _ fail: Bool) throws -> T {
  try checkedVoid(fail)
  return value
}

// Every result kind is only materialized after the error check, so no Swift
// value is constructed from, or destroyed in, uninitialized storage.
// CHECK-LABEL: swift::ThrowingResult<T_0_0> throwingGeneric(const T_0_0& value, bool fail)
// CHECK: if constexpr (std::is_base_of<::swift::_impl::RefCountedClass, T_0_0>::value) {
// CHECK-NEXT: void *returnValue;
// CHECK-NEXT: Functions::_impl::$s9Functions15throwingGenericyxx_SbtKlF(reinterpret_cast<void *>(&returnValue), {{.*}}, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return ::swift::_impl::implClassFor<T_0_0>::type::makeRetained(returnValue);
// CHECK-NEXT: } else if constexpr (::swift::_impl::isValueType<T_0_0>) {
// CHECK-NEXT: void *returnMetadata_ = swift::TypeMetadataTrait<T_0_0>::getTypeMetadata();
// CHECK: swift::_impl::OpaqueStorage returnStorage_(returnVWTable_->size, returnVWTable_->getAlignment());
// CHECK-NEXT: Functions::_impl::$s9Functions15throwingGenericyxx_SbtKlF(returnStorage_.getOpaquePointer(), {{.*}}, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return ::swift::_impl::implClassFor<T_0_0>::type::returnNewValue([&](void * _Nonnull returnValue) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: return ::swift::_impl::implClassFor<T_0_0>::type::initializeWithTake(reinterpret_cast<char * _Nonnull>(returnValue), returnStorage_.getOpaquePointer());
// CHECK: } else if constexpr (::swift::_impl::isSwiftBridgedCxxRecord<T_0_0>) {
// CHECK: Functions::_impl::$s9Functions15throwingGenericyxx_SbtKlF(storage, {{.*}}, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: T_0_0 result(static_cast<T_0_0 &&>(*storageObjectPtr));
// CHECK: } else {
// CHECK-NEXT: T_0_0 returnValue;
// CHECK-NEXT: Functions::_impl::$s9Functions15throwingGenericyxx_SbtKlF(reinterpret_cast<void *>(&returnValue), {{.*}}, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return returnValue;

// A fixed-layout value returned indirectly is written to stack storage.
// CHECK-LABEL: swift::ThrowingResult<LargeResult> throwingLarge(bool returnValue)
// CHECK: alignas(8) char returnStorage_[{{[0-9]+}}];
// CHECK-NEXT: Functions::_impl::$s9Functions13throwingLargeyAA0C6ResultVSbKF(returnStorage_, returnValue, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<LargeResult>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return Functions::_impl::_impl_LargeResult::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: Functions::_impl::_impl_LargeResult::initializeWithTake(result, returnStorage_);
// CHECK-NEXT: });
// CHECK-NEXT: }

// With library evolution, the layout of the struct can change, so the storage
// is sized at runtime, like the storage of the C++ class for the struct.
// RESILIENT-LABEL: swift::ThrowingResult<LargeResult> throwingLarge(bool returnValue)
// RESILIENT-NOT: char returnStorage_
// RESILIENT: void *returnMetadata_ = swift::TypeMetadataTrait<LargeResult>::getTypeMetadata();
// RESILIENT: swift::_impl::OpaqueStorage returnStorage_(returnVWTable_->size, returnVWTable_->getAlignment());
// RESILIENT-NEXT: Functions::_impl::$s9Functions13throwingLargeyAA0C6ResultVSbKF(returnStorage_.getOpaquePointer(), returnValue, _ctx, &opaqueError);
// RESILIENT-NEXT: if (opaqueError != nullptr)
// RESILIENT: Functions::_impl::_impl_LargeResult::initializeWithTake(result, returnStorage_.getOpaquePointer());

// A value whose layout isn't known statically is written to heap storage.
// CHECK-LABEL: swift::ThrowingResult<swift::Optional<LargeResult>> throwingOptionalLarge(bool returnValue)
// CHECK: void *returnMetadata_ = swift::TypeMetadataTrait<swift::Optional<LargeResult>>::getTypeMetadata();
// CHECK: swift::_impl::OpaqueStorage returnStorage_(returnVWTable_->size, returnVWTable_->getAlignment());
// CHECK-NEXT: Functions::_impl::$s9Functions21throwingOptionalLargeyAA0D6ResultVSgSbKF(returnStorage_.getOpaquePointer(), returnValue, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return swift::Expected<swift::Optional<LargeResult>>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return swift::_impl::_impl_Optional<LargeResult>::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: swift::_impl::_impl_Optional<LargeResult>::initializeWithTake(result, returnStorage_.getOpaquePointer());

// CHECK-LABEL: swift::ThrowingResult<RefResult> throwingRef(bool returnValue)
// CHECK: void *returnValue_ = Functions::_impl::$s9Functions11throwingRefyAA0C6ResultCSbKF(returnValue, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return swift::Expected<RefResult>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return _impl::_impl_RefResult::makeRetained(returnValue_);

// CHECK-LABEL: swift::ThrowingResult<SmallResult> throwingSmall(bool returnValue)
// CHECK: auto returnValue_ = Functions::_impl::$s9Functions13throwingSmallyAA0C6ResultVSbKF(returnValue, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return swift::Expected<SmallResult>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return Functions::_impl::_impl_SmallResult::returnNewValue([&](char * _Nonnull result) SWIFT_INLINE_THUNK_ATTRIBUTES {
// CHECK-NEXT: Functions::_impl::swift_interop_returnDirect_Functions_{{.*}}(result, returnValue_);

// CHECK-LABEL: swift::ThrowingResult<swift::String> throwingString(bool fail)
// CHECK: auto returnValue_ = Functions::_impl::$s9Functions14throwingStringySSSbKF(fail, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return swift::_impl::_impl_String::returnNewValue(

// CHECK-LABEL: swift::ThrowingResult<LargeResult> LargeResult::init(bool fail)
// CHECK: char returnStorage_[
// CHECK-NEXT: Functions::_impl::$s9Functions11LargeResultVyACSbKcfC(returnStorage_, fail, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: Functions::_impl::_impl_LargeResult::initializeWithTake(result, returnStorage_);

// CHECK-LABEL: swift::ThrowingResult<RefResult> RefResult::init(bool fail)
// CHECK: void *returnValue_ = Functions::_impl::$s9Functions9RefResultCyACSbKcfC(fail, {{.*}}, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return _impl::_impl_RefResult::makeRetained(returnValue_);

// CHECK-LABEL: swift::ThrowingResult<SmallResult> RefResult::small(bool fail)
// CHECK: auto returnValue_ = Functions::_impl::$s9Functions9RefResultC5smallyAA05SmallC0VSbKF(fail, {{.*}}, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: Functions::_impl::swift_interop_returnDirect_Functions_{{.*}}(result, returnValue_);

// CHECK-LABEL: swift::ThrowingResult<SmallResult> SmallResult::init(swift::Int value, bool fail)
// CHECK: auto returnValue_ = Functions::_impl::$s9Functions11SmallResultVyACSi_SbtKcfC(value, fail, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: Functions::_impl::swift_interop_returnDirect_Functions_{{.*}}(result, returnValue_);

// CHECK-LABEL: swift::ThrowingResult<SmallResult> SmallResult::doubled(bool fail) const
// CHECK: auto returnValue_ = Functions::_impl::$s9Functions11SmallResultV7doubledyACSbKF(fail, {{.*}}, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: Functions::_impl::swift_interop_returnDirect_Functions_{{.*}}(result, returnValue_);

// CHECK-LABEL: swift::ThrowingResult<SmallResult> SmallResult::make(bool fail)
// CHECK: auto returnValue_ = Functions::_impl::$s9Functions11SmallResultV4makeyACSbKFZ(fail, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: Functions::_impl::swift_interop_returnDirect_Functions_{{.*}}(result, returnValue_);

@_expose(Cxx)
public final class VoidMethods {
  public init() {}
  public func checked(_ fail: Bool) throws { try checkedVoid(fail) }
  public func never() throws -> Never { throw NaiveErrors.throwError }
}

// Class self supplies the context; don't emit an unused placeholder.
// CHECK: swift::ThrowingResult<void> VoidMethods::checked
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NOT: void* _ctx
// CHECK: if (opaqueError != nullptr)
// CHECK: return swift::Expected<void>();

// A throwing Never method must not return a successful void result.
// CHECK-LABEL: swift::ThrowingResult<void> VoidMethods::never() SWIFT_NORETURN_EXCEPT_ERRORS {
// CHECK: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: abort();
// CHECK-NEXT: }
