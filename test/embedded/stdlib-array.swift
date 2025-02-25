// RUN: %swift_frontend_plain -target armv7-apple-none-macho -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s
// RUN: %swift_frontend_plain -target arm64-apple-none-macho -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// UNSUPPORTED: CPU=wasm32
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

public func test() {
  var array: [Int] = [1, 2, 3]
  array.append(42)
  array.append(contentsOf: [4, 5, 6])
  array = array.sorted()
  array.sort()
  array.allSatisfy { $0 > 0 }
  array.contains { $0 > 0 }
  array.map { $0 * 2 }
  array.filter { $0 > 0 }
  array.dropFirst().dropLast().drop(while: { $0 < 0 })
  array.firstIndex(of: 42)
  array.min()
  array.max()
  array.partition(by: { $0 > 0 })
  array.reduce(0, +)
  array.shuffle()
  array = array.shuffled()
  array.randomElement()
}

test()

// CHECK: define {{.*}}i32 @{{_*}}main{{.*}}(i32 %0, ptr %1)
