// RUN: %target-run-simple-swift(   -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none %target-embedded-posix-shim -enable-experimental-feature EmbeddedKeyPaths) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none %target-embedded-posix-shim -enable-experimental-feature EmbeddedKeyPaths) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedKeyPaths

struct MyStruct {
  var x: Int8
  var y: Int16
  var z: Int32
}

@inline(never)
public func getValueIn<Root, Value>(_ root: Root, keyPath: KeyPath<Root, Value>) -> Value {
  return root[keyPath: keyPath]
}

let ms = MyStruct(x: 17, y: 25, z: 42)
print(getValueIn(ms, keyPath: \.x)) // CHECK: 17
print(getValueIn(ms, keyPath: \.y)) // CHECK: 25
print(getValueIn(ms, keyPath: \.z)) // CHECK: 42
