// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-silgen -I %t/Inputs -cxx-interoperability-mode=default -target %target-swift-5.1-abi-triple %t/test.swift | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: concurrency

//--- Inputs/header.h

#import <Foundation/Foundation.h>

struct NonTrivial {
  NonTrivial();
  NonTrivial(const NonTrivial &);
  ~NonTrivial();
};

@interface Producer : NSObject
- (void)produceWithCompletionHandler:(void (^_Nonnull)(struct NonTrivial))completion;
@end

//--- Inputs/module.modulemap

module ObjCxxAsync {
    header "header.h"
    requires cplusplus
    export *
}

//--- test.swift

import ObjCxxAsync

public func callIt(_ p: Producer) async {
  let x = await p.produce()
  _ = x
}

// CHECK-LABEL: sil {{.*}}@$sSo10NonTrivialVIeyBhX_ABT{{[zZ]}}_ : $@convention(c) @Sendable (@inout_aliasable @block_storage Any, @in_cxx NonTrivial) -> () {
// CHECK: bb0(%0 : $*@block_storage Any, [[ARG:%.*]] : $*NonTrivial):
// CHECK-NOT: destroy_addr [[ARG]]
// CHECK: copy_addr [[ARG]] to [init]
// CHECK-NOT: destroy_addr [[ARG]]
// CHECK: return
