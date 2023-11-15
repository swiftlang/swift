// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -strict-concurrency=complete \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency

//--- Test.h
#define NS_SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))

#pragma clang assume_nonnull begin

@import Foundation;

NS_SWIFT_SENDABLE
@protocol MyObjCProtocol <NSObject>
@property (readonly, nonatomic) NSString *test;
@end

#pragma clang assume_nonnull end

//--- main.swift
struct MyStruct: Sendable {
  let value: any MyObjCProtocol // Ok
}

extension Sendable {
  func compute() {}
}

extension MyObjCProtocol {
  func test() {
    compute() // Ok
  }
}
