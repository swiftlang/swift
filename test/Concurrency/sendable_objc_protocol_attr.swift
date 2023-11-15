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

@protocol MyRefinedObjCProtocol <MyObjCProtocol>
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

class K : NSObject, MyObjCProtocol {
  // expected-warning@-1 {{non-final class 'K' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  let test: String = "k"
}

class UncheckedK : NSObject, MyObjCProtocol, @unchecked Sendable { // ok
  let test: String = "unchecked"
}

class KRefined : NSObject, MyRefinedObjCProtocol {
  // expected-warning@-1 {{non-final class 'KRefined' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  let test: String = "refined"
}

class KUncheckedRefined : NSObject, MyRefinedObjCProtocol, @unchecked Sendable { // Ok
  let test: String = "refined unchecked"
}

do {
  func testSendable<T: Sendable>(_: T) {}

  testSendable(K()) // Ok
  testSendable(UncheckedK()) // Ok
  testSendable(KRefined()) // Ok
  testSendable(KUncheckedRefined()) // Ok
}
