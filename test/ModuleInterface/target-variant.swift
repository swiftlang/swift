// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/TargetVariant.swiftinterface) %s -module-name TargetVariant -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi
// RUN: %target-swift-typecheck-module-from-interface(%t/TargetVariant.swiftinterface) -module-name TargetVariant
// RUN: %FileCheck %s < %t/TargetVariant.swiftinterface

// REQUIRES: OS=macosx

// CHECK: swift-module-flags
// CHECK-SAME: -target {{.*}}-apple-macosx13
// CHECK-SAME: -target-variant {{.*}}-apple-ios16-macabi

// CHECK: public func test()
public func test() {}
