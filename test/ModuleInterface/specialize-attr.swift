// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// R/UN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s --implicit-check-not "\$SpecializeAttributeWithAvailability" < %t.swiftinterface

// CHECK: @_specialize(exported: false, kind: full, where T == Swift.Double)
// CHECK: public func specialize<T>(_ t: T)
@_specialize(exported: false, where T == Double)
public func specialize<T>(_ t: T) {}

// CHECK: @_specialize(exported: true, kind: full, availability: macOS, introduced: 12; where T == Swift.Int)
// CHECK: public func specializeWithAvailability<T>(_ t: T)
@_specialize(exported: true, availability: macOS 12, *; where T == Int)
public func specializeWithAvailability<T>(_ t: T) {}

// CHECK: @_specialize(exported: true, kind: full, availability: macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *; where T == Swift.Int)
// CHECK: public func specializeWithStdlibAvailability<T>(value: T) async
@_specialize(exported: true, availability: SwiftStdlib 5.1, *; where T == Int)
public func specializeWithStdlibAvailability<T>(value: T) async {}
