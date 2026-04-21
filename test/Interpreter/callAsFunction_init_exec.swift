// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

struct S1: CustomStringConvertible {
  var x: Int

  init(_ x: Int = 1) {
    self.x = x
  }
  func callAsFunction(_ y: Int = 2, _ fn: () -> Int) -> S1 {
    S1(x + y + fn())
  }
  var description: String {
    "S1: \(x)"
  }
}

struct S2 {
  var x: Int

  init(_ x: Int = 1) {
    self.x = x
  }
  func callAsFunction(_ y: Int = 2, _ fn: () -> Int) -> String {
    "S2: \(x + y + fn())"
  }
}

print(S1() { 4 })
// CHECK: S1: 7
print(S1.init() { 8 })
// CHECK: S1: 11
print(.init() { 16 } as S1)
// CHECK: S1: 19
print(S1(32) { 1 })
// CHECK: S1: 35
print(S1.init(64) { 1 })
// CHECK: S1: 67
print(.init(128) { 1 } as S1)
// CHECK: S1: 131

print(S2() { 4 })
// CHECK: S2: 7
print(S2.init() { 8 })
// CHECK: S2: 11
print(S2(32) { 1 })
// CHECK: S2: 35
print(S2.init(64) { 1 })
// CHECK: S2: 67
