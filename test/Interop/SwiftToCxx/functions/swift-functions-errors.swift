// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -enable-experimental-cxx-interop -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -Wno-shadow -Wno-unused-function)

// CHECK-LABEL: namespace Functions __attribute__((swift_private)) {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions18emptyThrowFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // emptyThrowFunction()
// CHECK: SWIFT_EXTERN void $s9Functions13throwFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunction()

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

// CHECK: inline void emptyThrowFunction() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18emptyThrowFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (Swift::Error(opaqueError))
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

// CHECK: inline void testDestroyedError() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18testDestroyedErroryyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (Swift::Error(opaqueError))
// CHECK: }

@_expose(Cxx)
public func throwFunction() throws {
    print("passThrowFunction")
    throw NaiveErrors.throwError
}

// CHECK: inline void throwFunction() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions13throwFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (Swift::Error(opaqueError))
// CHECK: }

@_expose(Cxx)
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
// CHECK: throw (Swift::Error(opaqueError))
// CHECK: return returnValue;
// CHECK: }
