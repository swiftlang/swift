// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-experimental-concurrency | %FileCheck %s

// CHECK: "$s5async1fyyYF"
public func f() async { }

// CHECK: "$s5async1gyyYKF"
public func g() async throws { }

