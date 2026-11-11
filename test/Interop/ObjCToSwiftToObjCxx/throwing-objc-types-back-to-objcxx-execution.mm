// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t%{fs-sep}use-objc-types.swift -module-name UseObjCTy -typecheck -verify \
// RUN:   -emit-clang-header-path %t%{fs-sep}UseObjCTy.h -enable-experimental-cxx-interop -clang-header-expose-decls=all-public \
// RUN:   -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX
// RUN: %FileCheck %s --input-file %t%{fs-sep}UseObjCTy.h

// RUN: %target-interop-build-clangxx -std=c++20 -fobjc-arc -c %t%{fs-sep}use-swift-objc-types.mm -I %t -o %t%{fs-sep}exceptions.o \
// RUN:   -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %t%{fs-sep}use-objc-types.swift -o %t%{fs-sep}exceptions -Xlinker %t%{fs-sep}exceptions.o \
// RUN:   -module-name UseObjCTy -Xfrontend -entry-point-function-name -Xfrontend swiftMain \
// RUN:   -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX
// RUN: %target-codesign %t%{fs-sep}exceptions
// RUN: %target-run %t%{fs-sep}exceptions

// RUN: %target-interop-build-clangxx -std=c++20 -fobjc-arc -fno-exceptions -c %t%{fs-sep}use-swift-objc-types.mm -I %t -o %t%{fs-sep}no-exceptions.o \
// RUN:   -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift %t%{fs-sep}use-objc-types.swift -o %t%{fs-sep}no-exceptions -Xlinker %t%{fs-sep}no-exceptions.o \
// RUN:   -module-name UseObjCTy -Xfrontend -entry-point-function-name -Xfrontend swiftMain \
// RUN:   -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX
// RUN: %target-codesign %t%{fs-sep}no-exceptions
// RUN: %target-run %t%{fs-sep}no-exceptions

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX
// UNSUPPORTED: CPU=arm64e

//--- use-objc-types.swift
import Foundation

enum ObjCResultError: Error { case failure }

public func throwingObject(_ fail: Bool) throws -> NSObject {
  if fail { throw ObjCResultError.failure }
  return NSObject()
}

public func throwingOptionalObject(_ fail: Bool) throws -> NSObject? {
  if fail { throw ObjCResultError.failure }
  return NSObject()
}

//--- use-swift-objc-types.mm

#include "UseObjCTy.h"
#include <assert.h>

// Checks that a successful call returns a live object that is released once
// it is no longer used, and that a failed call propagates the error.
template <class Operation> void checkResult(Operation operation) {
  __weak NSObject *weakObject = nil;
  @autoreleasepool {
#ifdef __cpp_exceptions
    NSObject *object = operation(false);
    try {
      (void)operation(true);
      assert(false && "the Swift error must propagate");
    } catch (const swift::Error &) {
    }
#else
    NSObject *object = operation(false).value();
    assert(!operation(true).has_value());
#endif
    weakObject = object;
    assert(weakObject != nil);
  }
  assert(weakObject == nil);
}

int main() {
  checkResult(UseObjCTy::throwingObject);
  checkResult(UseObjCTy::throwingOptionalObject);
  return 0;
}

// CHECK-LABEL: swift::ThrowingResult<NSObject *_Nonnull> throwingObject(bool fail)
// CHECK: void *returnValue_ = (__bridge void *)UseObjCTy::_impl::$s9UseObjCTy14throwingObjectySo8NSObjectCSbKF(fail, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<NSObject *_Nonnull>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: return (__bridge_transfer NSObject *)returnValue_;

// CHECK-LABEL: swift::ThrowingResult<NSObject *_Nullable> throwingOptionalObject(bool fail)
// CHECK: void *returnValue_ = (__bridge void *)UseObjCTy::_impl::$s9UseObjCTy22throwingOptionalObjectySo8NSObjectCSgSbKF(fail, _ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK: return (__bridge_transfer NSObject *)returnValue_;
