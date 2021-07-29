// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 | %FileCheck %s
// REQUIRES: concurrency

@available(SwiftStdlib 5.5, *)
public actor A {
  // CHECK: sil{{.*}} [ossa] @$s4test1AC6methodyyF
  public func method() { }
}

// CHECK: sil{{.*}} [ossa] @$s4test13takesIsolatedyyAA1ACYiF
@available(SwiftStdlib 5.5, *)
public func takesIsolated(_: isolated A) { }
