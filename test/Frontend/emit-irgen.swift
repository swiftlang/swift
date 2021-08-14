// RUN: %target-swift-frontend -emit-irgen %s | %FileCheck %s

// CHECK: define{{.*}} swiftcc void @"$s4main5helloyyF"
public func hello() { }
