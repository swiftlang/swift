// RUN: %target-run-simple-swift | FileCheck %s
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
let two_one = Array(lazy([1, 2, 3, 4]).reverse().filter { $0 % 2 == 0 }.map { $0 / 2 })
print(two_one)
// CHECK: [2, 1]

// rdar://problem/18208283
func flatten<Element, Sequence: SequenceType, InnerSequence: SequenceType
       where Sequence.Generator.Element == InnerSequence, InnerSequence.Generator.Element == Element> (outerSequence: Sequence) -> [Element] {
  var result = [Element]()

  for innerSequence in outerSequence {
    result.extend(innerSequence)
  }

  return result
}

// CHECK: [1, 2, 3, 4, 5, 6]
let flat = flatten([[1,2,3], [4,5,6]])
print(flat)

// rdar://problem/19416848
func observe<T:SequenceType, V where V == T.Generator.Element>(g:T) { }
observe(["a":1])
