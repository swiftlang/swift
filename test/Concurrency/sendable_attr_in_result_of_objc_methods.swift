// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 5 \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -strict-concurrency=complete \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency

//--- Test.h
#define SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))

#pragma clang assume_nonnull begin

@import Foundation;

@interface Test: NSObject
- (SWIFT_SENDABLE id)sendableReturnValue1;
- (id SWIFT_SENDABLE)sendableReturnValue2;
- (id)sendableReturnValue3 SWIFT_SENDABLE;
@end

#pragma clang assume_nonnull end

//--- main.swift
func test(test: Test) {
  let result1 = test.sendableReturnValue1()
  let result2 = test.sendableReturnValue2()
  let result3 = test.sendableReturnValue2()

  Task.detached {
    _ = result1 // Ok
    _ = result2 // Ok
    _ = result3 // Ok
  }
}
