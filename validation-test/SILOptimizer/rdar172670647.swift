// RUN: %target-run-simple-swift(-O) 

// REQUIRES: executable_test

struct S {
  var c: ContiguousArray<Int>
  var a: ContiguousArray<UInt64>
  var b: ContiguousArray<UInt64>

  init(n: Int) {
    c = .init(repeating: 0, count: n)
    a = .init(repeating: 0, count: n)
    b = .init(repeating: 0, count: n)
  }

  mutating func bump(n: Int) {
    do {
      var s = a.mutableSpan
      for i in 0 ..< n { s[i] &+= 1 }
    }
    do {
      var s = b.mutableSpan
      for i in 0 ..< n { s[i] &+= 1 }
    }
  }
}

@inline(never)
func run() -> (UInt64, UInt64) {
  var x = S(n: 3)
  let snapshot = Array(x.b)
  x.bump(n: 3)

  return (x.b[0], snapshot[0])
}

let (a, b) = run()
precondition(a != b)

