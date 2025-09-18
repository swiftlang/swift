// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name attrs \
// RUN:  -enable-experimental-feature InlineAlways \
// RUN:  -emit-private-module-interface-path %t.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name attrs \
// RUN:  -enable-experimental-feature InlineAlways
// RUN: %target-swift-typecheck-module-from-interface(%t.private.swiftinterface) -module-name attrs \
// RUN:  -enable-experimental-feature InlineAlways

// RUN: %FileCheck %s --check-prefixes CHECK --input-file %t.swiftinterface
// RUN: %FileCheck %s --check-prefixes CHECK --input-file %t.private.swiftinterface

// REQUIRES: swift_feature_InlineAlways

// CHECK: #if compiler(>=5.3) && $InlineAlways
// CHECK: @inline(always) public func inlineAlwaysFunc() {}
// CHECK: #else
// CHECK: @inline(__always) @inlinable public func inlineAlwaysFunc() {}
// CHECK: #endif
@inline(always)
public func inlineAlwaysFunc() {}
