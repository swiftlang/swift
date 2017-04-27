// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -typecheck %s -parse-as-library -emit-objc-header-path %t/swift.h
// RUN: %FileCheck %s < %t/swift.h

// REQUIRES: objc_interop

import Foundation

// CHECK: @import Dispatch;

// CHECK-LABEL: @interface Test : NSObject{{$}}
public class Test : NSObject { 
  // CHECK-NEXT: - (void)thank:(dispatch_queue_t _Nonnull)queue;
  @objc public func thank(_ queue: DispatchQueue) {}
  // CHECK-NEXT: init
} // CHECK-NEXT: @end
