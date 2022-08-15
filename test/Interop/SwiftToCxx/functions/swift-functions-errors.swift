// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK-LABEL: namespace Functions {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions18emptyThrowFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _self, SWIFT_ERROR_RESULT void ** _error) SWIFT_CALL; // emptyThrowFunction()
// CHECK: SWIFT_EXTERN void $s9Functions13throwFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _self, SWIFT_ERROR_RESULT void ** _error) SWIFT_CALL; // throwFunction()

// CHECK: }

enum NaiveErrors : Error {
    case returnError
    case throwError
}

public func emptyThrowFunction() throws { print("passEmptyThrowFunction") }

// CHECK: inline void emptyThrowFunction() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* self = nullptr;
// CHECK: _impl::$s9Functions18emptyThrowFunctionyyKF(self, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (swift::_impl::NaiveException("Exception"));
// CHECK: }

public func throwFunction() throws {
    print("passThrowFunction")
    throw NaiveErrors.throwError
}

// CHECK: inline void throwFunction() {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* self = nullptr;
// CHECK: _impl::$s9Functions13throwFunctionyyKF(self, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (swift::_impl::NaiveException("Exception"));
// CHECK: }

public func throwFunctionWithReturn() throws -> Int {
    print("passThrowFunctionWithReturn")
    throw NaiveErrors.returnError
    return 0
}

// CHECK: inline swift::Int throwFunctionWithReturn() SWIFT_WARN_UNUSED_RESULT {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* self = nullptr;
// CHECK: auto returnValue = _impl::$s9Functions23throwFunctionWithReturnSiyKF(self, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: throw (swift::_impl::NaiveException("Exception"));
// CHECK: return returnValue;
// CHECK: }