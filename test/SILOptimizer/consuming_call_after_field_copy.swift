// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

// Check that the copy of `x.s` is not replaced by a borrow which overlaps the consuming call `x.consume()`.

struct M: ~Copyable {
  var a: [UInt32] = [0]
  private let b: Any = ()
}

struct S {
  var a: [UInt32] = []
}

struct X: ~Copyable {
  var m: M
  var s = S()

  init(_ n: Int) {
    m = M()
    for i in 0..<n { s.a.append(UInt32(i)) }
  }

  consuming func consume() -> M {
    m.a.append(UInt32(s.a.count))
    return m
  }
}

@inline(never)
func sum(_ s: S) -> UInt32 { s.a.reduce(0, &+) }

@inline(never)
func test(_ n: Int) {
  let x = X(n)
  let s = x.s
  let m = x.consume()
  // CHECK: 499500 2
  print(sum(s), m.a.count)
}

test(Int(CommandLine.arguments.count) * 1000)
