// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/sendable_first.swift \
// RUN:   -import-objc-header %t/src/SendableFirst.h \
// RUN:   -swift-version 6 \
// RUN:   -strict-concurrency=complete \
// RUN:   -enable-experimental-feature SendableCompletionHandlers \
// RUN:   -module-name main -verify

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/ui_actor_first.swift \
// RUN:   -import-objc-header %t/src/UIActorFirst.h \
// RUN:   -swift-version 6 \
// RUN:   -strict-concurrency=complete \
// RUN:   -enable-experimental-feature SendableCompletionHandlers \
// RUN:   -module-name main -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_SendableCompletionHandlers

//--- SendableFirst.h
#define SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))
#define SWIFT_UI_ACTOR __attribute__((swift_attr("@UIActor")))

#pragma clang assume_nonnull begin

@import Foundation;

@protocol SwiftAttrCollisionSendableFirst
- (void)firstWithCompletionHandler:(SWIFT_SENDABLE void (^)(int result))completionHandler;
@end

@protocol SwiftAttrCollisionUIActorSecond
@optional
- (void)secondWithCompletionHandler:(SWIFT_UI_ACTOR void (^)(int result))completionHandler;
@end

#pragma clang assume_nonnull end

//--- sendable_first.swift
import Foundation

// expected-no-diagnostics

final class SwiftAttrCollisionUIActorSecondWitness: NSObject, SwiftAttrCollisionUIActorSecond {
  func second(completionHandler: @MainActor @Sendable (Int32) -> Void) {}
}

//--- UIActorFirst.h
#define SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))
#define SWIFT_UI_ACTOR __attribute__((swift_attr("@UIActor")))

#pragma clang assume_nonnull begin

@import Foundation;

@protocol SwiftAttrCollisionUIActorFirst
- (void)firstWithCompletionHandler:(SWIFT_UI_ACTOR void (^)(int result))completionHandler;
@end

@protocol SwiftAttrCollisionSendableSecond
@optional
- (void)secondWithCompletionHandler:(SWIFT_SENDABLE void (^)(int result))completionHandler;
@end

#pragma clang assume_nonnull end

//--- ui_actor_first.swift
import Foundation

// expected-no-diagnostics

final class SwiftAttrCollisionSendableSecondWitness: NSObject, SwiftAttrCollisionSendableSecond {
  func second(completionHandler: @Sendable (Int32) -> Void) {}
}
