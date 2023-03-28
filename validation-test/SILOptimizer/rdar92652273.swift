// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_test_mode_optimize

struct Consumer {
  let store: [Int]
  @inline(__always)
  init(_ x: [Int]) {
    var y = x
    y[0] = 42
    store = y
  }
}

typealias PInt = UnsafePointer<Int>
@inline(never)
func baseAddress(_ a: PInt) -> PInt { a }

extension Optional {
  @inline(__always)
  mutating func release() -> Wrapped {
    defer { self = nil }
    return self!
  }
}

@inline(never)
func main(_ a: inout [Int]?) -> [Int] {
  let x = a.release()
  let r = Consumer(x).store
  return r
}

func test() {
  var a = Optional(Array(0...3))
  let b0 = baseAddress(a!)
  let b = main(&a)
  let b1 = baseAddress(b)
  // CHECK: true
  print(b0 == b1)
  // CHECK: nil [42, 1, 2, 3]
  print(a as Any, b)
}

test()

