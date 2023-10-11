// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

public struct MyStruct {
}

public func main() {
  var arr: [MyStruct] = []
  var arr2: [MyStruct] = [.init()]
  var arr3 = arr2
  arr3.append(MyStruct())
}

public func copy(_ a: inout [MyStruct]) {
  var a = a
}

// CHECK: define {{.*}}@"$s4mainAAyyF"
// CHECK: define {{.*}}@"$s4main4copyyySayAA8MyStructVGzF"
