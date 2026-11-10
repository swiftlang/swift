// REQUIRES: swift_feature_Embedded

// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface | %FileCheck %s --implicit-check-not='WP" = external'
// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded -DEXPLICIT_EXPORT | %FileCheck %s --implicit-check-not='WP" = external'

// A conditional conformance whose conditions make all generic parameters
// concrete, e.g. 'extension Box: Q where T == UInt8', is effectively
// non-generic, so its witness table is emitted eagerly. The witness tables of
// its base conformances, which are specializations of generic conformances,
// must be emitted too.

public protocol P {
  associatedtype Element
  func first() -> Element
}

public protocol Q: P where Element == UInt8 {}

public struct Box<T>: P {
  public var value: T
  public init(value: T) { self.value = value }
  public func first() -> T { value }
}

#if EXPLICIT_EXPORT
@export(interface)
#endif
extension Box: Q where T == UInt8 {}

// CHECK-DAG: @"$e5Repro3BoxVyxGAA1QAAs5UInt8VRszlWP" = {{.*}}{{global|constant}} {{.*}}@"$e5Repro3BoxVys5UInt8VGAA1PAAWP{{(\.ptrauth(\.[0-9]+)?)?}}"
// CHECK-DAG: @"$e5Repro3BoxVys5UInt8VGAA1PAAWP" = {{.*}}{{global|constant}} [
