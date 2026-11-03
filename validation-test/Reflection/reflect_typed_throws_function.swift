// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_typed_throws_function
// RUN: %target-codesign %t/reflect_typed_throws_function

// RUN: %target-run %target-swift-reflection-test %t/reflect_typed_throws_function | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// XFAIL: *

// REMOTE-MIRRORS-GAP-BEGIN
// Title: typed throws thrown-error type omitted from FunctionTypeRef dump
// Status: XFAIL
// RemoteInspection-Reference: stdlib/public/RemoteInspection/TypeRef.cpp:156-270 — visitFunctionTypeRef prints convention, differentiability, global-actor, isolated-any, sending-result, execution, parameters, and result, but never emits the thrownError stored on the FunctionTypeRef.
// Runtime-Reference: include/swift/RemoteInspection/TypeRef.h:555-633 — FunctionTypeRef::ThrownError storage and getThrownError() accessor are populated by createFunctionType when the function metadata carries a typed-throws error type, but the dump path drops them on the floor.
// Description:
//   FunctionTypeRef carries a ThrownError TypeRef that is set when the
//   function type uses typed throws (e.g. `throws(MyError)`). The function
//   metadata in the inferior tracks the thrown-error type pointer when the
//   typed-throws bit is set, and createFunctionType propagates it onto the
//   reconstructed FunctionTypeRef. visitFunctionTypeRef in the dump path,
//   however, never prints it, so reflecting a typed-throws function value
//   omits the error type entirely. Closing the gap means emitting a
//   `(thrown_error <typeref>)` child whenever getThrownError() is non-null.
// REMOTE-MIRRORS-GAP-END

import SwiftReflectionTest

struct MyError: Error {}

@available(macOS 15.0, *)
func makeTypedThrowingFn() -> () throws(MyError) -> Void {
  return { throw MyError() }
}

if #available(macOS 15.0, *) {
  reflect(any: makeTypedThrowingFn())
}

// CHECK: Reflecting an existential.
// CHECK: Type reference:
// CHECK: (function
// CHECK: (thrown_error
// CHECK: (struct {{.*}}MyError

doneReflecting()

// CHECK: Done.
