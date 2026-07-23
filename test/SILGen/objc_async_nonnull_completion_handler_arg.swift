// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend -emit-silgen %t/src/main.swift -import-objc-header %t/src/Test.h -target %target-swift-5.1-abi-triple -module-name main -I %t/src | %FileCheck %t/src/main.swift

// REQUIRES: concurrency
// REQUIRES: objc_interop

// rdar://181185273

//--- Test.h

#import <Foundation/Foundation.h>

typedef void (^MockRegistrationCallback)(NSDictionary<NSString *, NSString *> * _Nonnull cachedEvents, NSError *error);

@protocol MockSubscriptionProviding <NSObject>
- (void)registerWithCompletion:(MockRegistrationCallback)completion;
@end

//--- main.swift

import Foundation

final class Repro: NSObject, MockSubscriptionProviding {
  func register() async throws -> [String : String] {
    [:]
  }
}

// CHECK-LABEL: sil shared [thunk] [ossa] @{{.*}}8registerSDyS2SGyYaKFyyYacfU_To
// CHECK: bb{{[0-9]+}}({{%.*}} : @guaranteed $@convention(block) @Sendable (NSDictionary, Optional<NSError>) -> ()):
// CHECK: enum $Optional<NSError>, #Optional.some!enumelt, {{%.*}}
// CHECK: [[NSERROR:%.*]] = begin_borrow {{%.*}}
// CHECK: apply {{%.*}}(undef, [[NSERROR]]) : $@convention(block) @Sendable (NSDictionary, Optional<NSError>) -> ()
