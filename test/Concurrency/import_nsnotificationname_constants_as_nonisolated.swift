// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -strict-concurrency=complete \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency

//--- Test.h
#define SWIFT_MAIN_ACTOR __attribute__((swift_attr("@MainActor")))

#pragma clang assume_nonnull begin

@import Foundation;

SWIFT_MAIN_ACTOR
@interface Test : NSObject
@end

extern NSNotificationName const TestDidTrigger __attribute__((swift_name("Test.didTrigger")));

SWIFT_MAIN_ACTOR
extern NSNotificationName const TestIsolatedTrigger __attribute__((swift_name("Test.isolatedTrigger")));

#pragma clang assume_nonnull end

//--- main.swift

func testAsync() async {
  print(Test.didTrigger) // Ok (property is nonisolated)
  print(Test.isolatedTrigger)
  // expected-warning@-1 {{main actor-isolated class property 'isolatedTrigger' cannot be accessed from outside of the actor}}
}

@MainActor
func testMainActor() {
  print(Test.didTrigger) // Ok
  print(Test.isolatedTrigger) // Ok
}
