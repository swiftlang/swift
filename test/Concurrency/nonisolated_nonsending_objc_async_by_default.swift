// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 6 \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop

//--- Test.h
#define MAIN_ACTOR __attribute__((__swift_attr__("@MainActor")))

#pragma clang assume_nonnull begin

@import Foundation;

@interface Test : NSObject
- (void)loadWithCompletionHandler:(void (^)(void)) completionHandler;
@end

MAIN_ACTOR
@interface TestIsolated : NSObject
- (void)loadWithCompletionHandler:(void (^)(void)) completionHandler;
@end

#pragma clang assume_nonnull end

//--- main.swift

class OverrideTest1 : Test {
  override func load() async {}
}

class OverrideTest2 : TestIsolated {
  override func load() async {}
}

func test(t: Test, i: TestIsolated) async throws {
  let fn = t.load // nonisolated(nonsending) () async -> Void

  let _: @isolated(any) () async -> Void = fn
  // expected-error@-1 {{cannot convert value of type 'nonisolated(nonsending) () async -> Void' to specified type '@isolated(any) () async -> Void'}}

  let isolatedFn = i.load
  let _: () -> Void = isolatedFn
  // expected-error@-1 {{invalid conversion from 'async' function of type '@MainActor @Sendable () async -> Void' to synchronous function type '() -> Void'}}
}

func testOverrides(o1: OverrideTest1, o2: OverrideTest2) {
  let _: () -> Void = o1.load
  // expected-error@-1 {{invalid conversion from 'async' function of type '() async -> ()' to synchronous function type '() -> Void'}}
  let _: () -> Void = o2.load
  // expected-error@-1 {{invalid conversion from 'async' function of type '@MainActor @Sendable () async -> ()' to synchronous function type '() -> Void'}}
}
