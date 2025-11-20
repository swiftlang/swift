// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/src/main.swift \
// RUN:   -module-name main -I %t/src -emit-sil -sil-verify-all | %FileCheck %s

//--- test.h

typedef long NSInteger;
typedef enum XCTestErrorCode: NSInteger {
    XCTestErrorCodeTimeoutWhileWaiting,
    XCTestErrorCodeFailureWhileWaiting,
} XCTestErrorCode;

static XCTestErrorCode const XCTestErrorUnsupported = (XCTestErrorCode)109;

//--- Framework.apinotes
---
Name: Framework
Tags:
- Name: XCTestErrorCode
  NSErrorDomain: XCTestErrorDomain

//--- module.modulemap
module Framework {
  header "test.h"
}

//--- main.swift
import Framework
func foo() {
  print(XCTestError.timeoutWhileWaiting)
  print(XCTestErrorUnsupported)
}


// CHECK: // XCTestErrorUnsupported.getter
// CHECK: sil shared [transparent] @$sSo22XCTestErrorUnsupportedSo0aB4CodeVvg : $@convention(thin) () -> XCTestError {
