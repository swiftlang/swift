// RUN: %target-swift-frontend %s -target x86_64-apple-macosx10.14 -target-sdk-version 10.15.4 -emit-ir | %FileCheck %s

// REQUIRES: OS=macosx

public func test() { }

// CHECK: "SDK Version", [3 x i32] [i32 10, i32 15, i32 4]
