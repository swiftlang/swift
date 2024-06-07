// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -strict-concurrency=complete \
// RUN:   -disable-availability-checking \
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
  // expected-warning@-1 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-2 {{property access is 'async'}}
}

@MainActor
func testMainActor() {
  print(Test.didTrigger) // Ok
  print(Test.isolatedTrigger) // Ok
}
