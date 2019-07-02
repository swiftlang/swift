// RUN: %target-run-simple-swift %s | %FileCheck %s

extension FixedWidthInteger: ExpressibleByUnicodeScalarLiteral {
  @_transparent
  public init(unicodeScalarLiteral value: Unicode.Scalar) {
    self = Self(value.value)
  }
  func foo() -> String {
    return "Foo!"
  }
}

let a: [Int16] = ["a", "b", "c", "d", "e"]
let b: [UInt32] = ["a", "b", "c", "d", "e"]

protocol P {
}
protocol Q {
  func foo2() -> String
}
extension P {//}: Q {
  func foo2() -> String {
    return "Foo2 \(self)!"
  }
}
class C {}
struct S {
  let a = 99
}
protocol P2: P {}
extension P2 {}
extension Numeric: P2 {}

extension C: P2 {}
extension S: P2 {}

// CHECK: Foo2 main.C!
print(C().foo2())
// CHECK: Foo2 S(a: 99)!
print(S().foo2())
// CHECK: Foo2 99!
print(Int8(99).foo2())
// CHECK: Foo2 99!
print(UInt32(99).foo2())
// CHECK: Foo!
print(Int8(99).foo())
// CHECK: Foo!
print(Int32(99).foo())
// CHECK: Foo!
print(UInt32(99).foo())

// CHECK: [97, 98, 99, 100, 101]
print(a)
// CHECK: [97, 98, 99, 100, 101]
print(b)
// CHECK: ["Foo2 97!", "Foo2 98!", "Foo2 99!", "Foo2 100!", "Foo2 101!"]
print(a.map {$0.foo2()})
// CHECK: ["Foo!", "Foo!", "Foo!", "Foo!", "Foo!"]
print(b.map {$0.foo()})
// CHECK: ["Foo2 97!", "Foo2 98!", "Foo2 99!", "Foo2 100!", "Foo2 101!"]
print(b.map {$0.foo2()})

public func use(_ value: Int) -> Int {
    return value + "1" // ← Used ExpressibleByUnicodeScalarLiteral
}
//public func use<T>(_ value: T) -> T
//  where T : FixedWidthInteger {
//    return value + "1" // ← Used ExpressibleByUnicodeScalarLiteral
//}

print(use(1))

let c: P2 = 99.0
// CHECK: Foo2 99.0!
print(c.foo2())
