// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx10.14 -target-sdk-version 10.15.4 -target-variant %target-cpu-apple-ios13.1-macabi -target-variant-sdk-version 13.4 -emit-ir | %FileCheck %s

// REQUIRES: OS=macosx

public func test() { }

// CHECK: "SDK Version", [3 x i32] [i32 10, i32 15, i32 4]
// CHECK: "darwin.target_variant.triple", !"{{.*}}-apple-ios13.1-macabi"
// CHECK: "darwin.target_variant.SDK Version", [2 x i32] [i32 13, i32 4]
