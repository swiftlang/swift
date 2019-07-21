// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

func fib() {
  var (a, b) = (0, 1)
  while b < 10 {
    print(b)
    (a, b) = (b, a+b)
  }
}
fib()

// CHECK: 1
// CHECK: 1
// CHECK: 2
// CHECK: 3
// CHECK: 5
// CHECK: 8

// From: <rdar://problem/17796401>
// FIXME: <rdar://problem/21993692> type checker too slow
let two_oneA = [1, 2, 3, 4].lazy.reversed()
let two_one = Array(two_oneA.filter { $0 % 2 == 0 }.map { $0 / 2 })
print(two_one)
// CHECK: [2, 1]

// rdar://problem/18208283
func flatten<Element, Seq: Sequence, InnerSequence: Sequence> (_ outerSequence: Seq) -> [Element]
    where Seq.Iterator.Element == InnerSequence, InnerSequence.Iterator.Element == Element {
  var result = [Element]()

  for innerSequence in outerSequence {
    result.append(contentsOf: innerSequence)
  }

  return result
}

// CHECK: [1, 2, 3, 4, 5, 6]
let flat = flatten([[1,2,3], [4,5,6]])
print(flat)

// rdar://problem/19416848
func observe<T:Sequence, V>(_ g:T) where V == T.Iterator.Element { }
observe(["a":1])
