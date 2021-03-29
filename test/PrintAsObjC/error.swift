// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module-path %t/error.swiftmodule -emit-objc-header-path %t/error.h -experimental-allow-module-with-compiler-errors %s
// RUN: %FileCheck %s < %t/error.h
// RUN: %check-in-clang %t/error.h

// REQUIRES: objc_interop

import Foundation

@objc class ErrorClass: NSObject {
// CHECK: @interface ErrorClass
  @objc let member: Int
  // CHECK: @property {{.*}} NSInteger member;
  @objc let invalidMember: undefined
  // TODO: Not output on invalid type

  @objc func method() {}
  // CHECK: - (void)method;

  @objc func methodParams(a: Int, b: Int) {}
  // CHECK: - (void)methodParamsWithA:(NSInteger)a b:(NSInteger)b;

  @objc class func classMethod() {}
  // CHECK: + (void)classMethod;

  @objc(objcMethod)
  func renamedMethod() {}
  // CHECK: - (void)objcMethod;

  @objc func invalidBody() {
  // CHECK: - (void)invalidBody;
    undefined
  }

  @objc func invalidRet() -> undefined {}
  // CHECK: - (/* error */id)invalidRet

  @objc func invalidParams(a: undefined) {}
  // TODO: Not output with invalid parameters

  @objc(invalid::)
  func invalidRenamedMethod() {}
  // CHECK: - (void)invalidRenamedMethod;

  @objc @undefined func invalidAttribute() {}
  // CHECK: - (void)invalidAttribute;

  @objc someundefinedmodifier func invalidModifier() {}
  // TODO: someundefinedmodifier treated as a function, so invalidModifier not seen as @objc

  @objc @available
  func invalidAvailability() {}
  // CHECK: - (void)invalidAvailability;
}

@objc class InvalidParent: undefined {}
// CHECK: @interface InvalidParent
