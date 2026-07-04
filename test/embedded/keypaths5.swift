// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedKeyPaths -wmo -o - | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedKeyPaths

public struct MyStruct {
  var x: Int32
  var y: Int32

  var max: Int32 {
    x > y ? x : y
  }
}

@inline(never)
public func getValueIn<Root, Value>(_ root: Root, keyPath: KeyPath<Root, Value>) -> Value {
  return root[keyPath: keyPath]
}

@inline(never)
public func getValueAsAnyIn<Root>(_ root: Root, keyPath: PartialKeyPath<Root>) -> Any {
  return root[keyPath: keyPath]
}

@inline(never)
public func getValueAsAnyIn<Root>(anyRoot root: Root, keyPath: AnyKeyPath) -> Any? {
  return root[keyPath: keyPath]
}

public func getKeyPaths(ms: MyStruct) {
  _ = getValueIn(ms, keyPath: \.x)
  _ = getValueAsAnyIn(ms, keyPath: \.x)
  _ = getValueAsAnyIn(anyRoot: ms, keyPath: \MyStruct.x)
}

// CHECK-LABEL: define linkonce_odr hidden swiftcc i32 @"$e9keypaths510getValueIn_7keyPathq_x_s03KeyF0Cyxq_Gtr0_lFAA8MyStructV_s5Int32VTg5"
// CHECK-NOT: ret void
// CHECK: call swiftcc i32 @"$e18swift_getAtKeyPath9keypaths58MyStructV_s5Int32VTg5"

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$e9keypaths515getValueAsAnyIn_7keyPathypx_s010PartialKeyH0CyxGtlFAA8MyStructV_Tg5"
// CHECK-NOT: ret void
// CHECK: call swiftcc void @"$e25swift_getAtPartialKeyPath9keypaths58MyStructV_Tg5"

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$e9keypaths515getValueAsAnyIn7anyRoot7keyPathypSgx_s0e3KeyJ0CtlFAA8MyStructV_Tg5"
// CHECK-NOT: ret void
// CHECK: call swiftcc void @"$e21swift_getAtAnyKeyPath9keypaths58MyStructV_Tg5"
