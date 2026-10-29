// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -Onone -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -O -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded


public func printPackElements<each T: BinaryInteger>(s: repeat each T) {
  for elem in repeat each s {
    printInt(elem)
  }
}

public struct S<each T: BinaryInteger> {
  let vals: (repeat each T)
}

@inline(never)
public func make() -> S<Int, Int8, Int> {
  return S(vals: (Int(1), Int8(2), Int(3)))
}

@inline(never)
func printInt<T: BinaryInteger>(_ n: T) {
  print(n)
}

public func printTupleElements<each T: BinaryInteger>(s: S<repeat each T>) {
  for elem in repeat each s.vals {
    print(elem)
  }
}

public func nestedPackElements<each T: BinaryInteger, each U: BinaryInteger>(ts: repeat each T, us: repeat each U) {
  for t in repeat each ts {
    for u in repeat each us {
      print("\(t) \(u)")
    }
  }
}

@main
struct Main {
  static func main() {
    // Homogeneous pack: the pack loop does not need to be unrolled.
    // CHECK: 1
    // CHECK: 2
    // CHECK: 3
    printPackElements(s: 1, 2, 3)

    // Non-homogeneous pack: the pack loop must be unrolled, even at -Onone.
    // CHECK: 1
    // CHECK: 2
    // CHECK: 3
    printPackElements(s: Int8(1), Int16(2), UInt32(3))

    // Empty pack: the body of the pack loop is unreachable.
    // CHECK: before empty pack
    // CHECK-NEXT: after empty pack
    print("before empty pack")
    printPackElements()
    print("after empty pack")

    // rdar://189863699: Embedded Swift: specialized pack-expansion tuple initializer crashes at -Onone
    let s = make()
    // CHECK: 2
    print(s.vals.1)

    // CHECK: 1
    // CHECK: 2
    // CHECK: 3
    printTupleElements(s: s)

    // CHECK: 1 4
    // CHECK: 1 5
    // CHECK: 1 6
    // CHECK: 2 4
    // CHECK: 2 5
    // CHECK: 2 6
    // CHECK: 3 4
    // CHECK: 3 5
    // CHECK: 3 6
    nestedPackElements(ts: Int8(1), Int16(2), UInt32(3), us: Int32(4), UInt16(5), UInt8(6))
  }
}
