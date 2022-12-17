// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -enable-experimental-cxx-interop -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY  -Wno-shadow -Wno-unused-function)

// CHECK-LABEL: namespace Functions __attribute__((swift_private)) {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions18emptyThrowFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // emptyThrowFunction()
// CHECK: SWIFT_EXTERN void $s9Functions18testDestroyedErroryyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // testDestroyedError()
// CHECK: SWIFT_EXTERN void $s9Functions13throwFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunction()
// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions31throwFunctionWithPossibleReturnyS2iKF(ptrdiff_t a, SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithPossibleReturn(_:)
// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions23throwFunctionWithReturnSiyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithReturn()


// CHECK: }

@_expose(Cxx)
public enum NaiveErrors : Error {
    case returnError
    case throwError

    public func getMessage() {
        print(self)
    }
}

@_expose(Cxx)
public func emptyThrowFunction() throws { print("passEmptyThrowFunction") }

// CHECK: inline Swift::ThrowingResult<void> emptyThrowFunction() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18emptyThrowFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (Swift::Error(opaqueError));
// CHECK: #else
// CHECK: return SWIFT_RETURN_THUNK(void, Swift::Error(opaqueError));
// CHECK: #endif
// CHECK: }

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

// CHECK: inline Swift::ThrowingResult<void> testDestroyedError() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18testDestroyedErroryyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (Swift::Error(opaqueError));
// CHECK: #else
// CHECK: return SWIFT_RETURN_THUNK(void, Swift::Error(opaqueError));
// CHECK: #endif
// CHECK: }

@_expose(Cxx)
public func throwFunction() throws {
    print("passThrowFunction")
    throw NaiveErrors.throwError
}

// CHECK: inline Swift::ThrowingResult<void> throwFunction() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions13throwFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (Swift::Error(opaqueError));
// CHECK: #else
// CHECK: return SWIFT_RETURN_THUNK(void, Swift::Error(opaqueError));
// CHECK: #endif
// CHECK: }

@_expose(Cxx)
public func throwFunctionWithPossibleReturn(_ a: Int) throws -> Int {
    print("passThrowFunctionWithPossibleReturn")
    if (a == 0) {
        throw NaiveErrors.returnError
    }
    return 0
}

// CHECK: inline Swift::ThrowingResult<swift::Int> throwFunctionWithPossibleReturn(swift::Int a) SWIFT_WARN_UNUSED_RESULT {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: auto returnValue = _impl::$s9Functions31throwFunctionWithPossibleReturnyS2iKF(a, _ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (Swift::Error(opaqueError));
// CHECK: #else
// CHECK: return SWIFT_RETURN_THUNK(swift::Int, Swift::Error(opaqueError));
// CHECK: #endif
// CHECK: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK: }

@_expose(Cxx)
public func throwFunctionWithReturn() throws -> Int {
    print("passThrowFunctionWithReturn")
    throw NaiveErrors.returnError
    return 0
}

// CHECK: inline Swift::ThrowingResult<swift::Int> throwFunctionWithReturn() SWIFT_WARN_UNUSED_RESULT {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: auto returnValue = _impl::$s9Functions23throwFunctionWithReturnSiyKF(_ctx, &opaqueError);
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (Swift::Error(opaqueError));
// CHECK: #else
// CHECK: return SWIFT_RETURN_THUNK(swift::Int, Swift::Error(opaqueError));
// CHECK: #endif
// CHECK: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK: }
