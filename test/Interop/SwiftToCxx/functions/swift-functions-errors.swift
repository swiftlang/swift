// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK-LABEL: namespace Functions {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions18emptyThrowFunctionyyKF(void) SWIFT_CALL; // emptyThrowFunction()
// CHECK: SWIFT_EXTERN void $s9Functions13throwFunctionyyKF(void) SWIFT_CALL; // throwFunction()

// CHECK: }

//XFAIL: *

enum NaiveErrors: Error {
    case returnError
    case throwError
}

public func emptyThrowFunction() throws { print("passEmptyThrowFunction") }

// CHECK: inline void emptyThrowFunction() {
// CHECK: return _impl::$s9Functions18emptyThrowFunctionyyKF();
// CHECK: }

public func throwFunction() throws {
    print("passThrowFunction")
    throw NaiveErrors.throwError
}

// CHECK: inline void throwFunction() {
// CHECK: return _impl::$s9Functions13throwFunctionyyKF();
// CHECK: }