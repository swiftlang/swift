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

// The loop body calls a protocol requirement directly on the pack element.
public func printBitWidths<each T: BinaryInteger>(s: repeat each T) {
  for elem in repeat each s {
    print(elem.bitWidth)
  }
}

public func sequentialPackLoops<each T: BinaryInteger>(s: repeat each T) {
  for elem in repeat each s {
    print(elem)
  }
  for elem in repeat each s {
    print(elem * 10)
  }
}

public func packLoopInWhileLoop<each T: BinaryInteger>(n: Int, s: repeat each T) {
  var i = 0
  while i < n {
    for elem in repeat each s {
      print("\(i) \(elem)")
    }
    i += 1
  }
}

public func siblingNestedPackLoops<each T: BinaryInteger, each U: BinaryInteger>(ts: repeat each T, us: repeat each U) {
  for t in repeat each ts {
    for u in repeat each us {
      print("\(t) \(u)")
    }
  }
  for u in repeat each us {
    for t in repeat each ts {
      print("\(u) \(t)")
    }
  }
}

public func zippedPackElements<each T: BinaryInteger, each U: BinaryInteger>(ts: repeat each T, us: repeat each U)
    where (repeat (each T, each U)): Any {
  for (t, u) in repeat (each ts, each us) {
    print("\(t) \(u)")
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

    // CHECK: 8
    // CHECK-NEXT: 16
    // CHECK-NEXT: 32
    printBitWidths(s: Int8(1), Int16(2), UInt32(3))

    // CHECK: 1
    // CHECK-NEXT: 2
    // CHECK-NEXT: 10
    // CHECK-NEXT: 20
    sequentialPackLoops(s: Int8(1), Int16(2))

    // CHECK: 0 1
    // CHECK-NEXT: 0 2
    // CHECK-NEXT: 1 1
    // CHECK-NEXT: 1 2
    packLoopInWhileLoop(n: 2, s: Int8(1), Int16(2))

    // CHECK: 1 3
    // CHECK-NEXT: 1 4
    // CHECK-NEXT: 2 3
    // CHECK-NEXT: 2 4
    // CHECK-NEXT: 3 1
    // CHECK-NEXT: 3 2
    // CHECK-NEXT: 4 1
    // CHECK-NEXT: 4 2
    siblingNestedPackLoops(ts: Int8(1), Int16(2), us: Int32(3), UInt8(4))

    // CHECK: 1 3
    // CHECK-NEXT: 2 4
    zippedPackElements(ts: Int8(1), Int16(2), us: Int32(3), UInt8(4))
  }
}
