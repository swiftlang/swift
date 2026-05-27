// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 5 \
// RUN:   -strict-concurrency=complete \
// RUN:   -enable-upcoming-feature NonisolatedNonsendingByDefault \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

//--- Test.h
#define SWIFT_NONISOLATED __attribute__((__swift_attr__("nonisolated")))

#pragma clang assume_nonnull begin

@import Foundation;

SWIFT_NONISOLATED
@interface Doc : NSObject
- (void)saveWithCompletionHandler:(void (^ __nullable)(BOOL success))completionHandler;
@end

SWIFT_NONISOLATED
@interface Doc(Reset)
- (void)resetWithCompletionHandler:(void (^ __nullable)(BOOL success))completionHandler;
@end

#pragma clang assume_nonnull end

//--- main.swift

final class Test {
  let doc = Doc()
  
  func test() async {
    _ = await doc.save() // Ok
    _ = await doc.reset() // Ok
  }
}
