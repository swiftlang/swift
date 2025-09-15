// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -Xcc -fptrauth-calls -module-name Swift | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// Some verification is blocked on string lookup succeeding.
struct String {}

public func test() { }

// CHECK-LABEL: define {{.*}}void @"$es4testyyF"()
