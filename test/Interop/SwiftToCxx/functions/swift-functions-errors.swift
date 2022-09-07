// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK-LABEL: namespace Functions {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions18emptyThrowFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // emptyThrowFunction()
// CHECK: SWIFT_EXTERN void $s9Functions13throwFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunction()

// CHECK: }

enum NaiveErrors : Error {
    case returnError
    case throwError
}

public func emptyThrowFunction() throws { print("passEmptyThrowFunction") }

// CHECK: inline void emptyThrowFunction() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18emptyThrowFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (swift::Error(opaqueError))
// CHECK: }

class TestDestroyed {
  deinit {
    print("Test destroyed")
  }
}

public struct DestroyedError : Error {
  let t = TestDestroyed()
}

public func testDestroyedError() throws { throw DestroyedError() }

// CHECK: inline void testDestroyedError() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18testDestroyedErroryyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (swift::Error(opaqueError))
// CHECK: }

public func throwFunction() throws {
    print("passThrowFunction")
    throw NaiveErrors.throwError
}

// CHECK: inline void throwFunction() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions13throwFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (swift::Error(opaqueError))
// CHECK: }

public func throwFunctionWithReturn() throws -> Int {
    print("passThrowFunctionWithReturn")
    throw NaiveErrors.returnError
    return 0
}

// CHECK: inline swift::Int throwFunctionWithReturn() SWIFT_WARN_UNUSED_RESULT {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: auto returnValue = _impl::$s9Functions23throwFunctionWithReturnSiyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (swift::Error(opaqueError))
// CHECK: return returnValue;
// CHECK: }
