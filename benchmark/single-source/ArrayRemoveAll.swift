// This test checks the performance of removeAll on a non-uniquely referenced array.
import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "ArrayRemoveAll_Class",
    runFunction: run_ArrayRemoveAll_Class,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(inputArray_Class) },
    tearDownFunction: { inputArray_Class = nil }
  ),
  BenchmarkInfo(
    name: "ArrayRemoveAll_Int",
    runFunction: run_ArrayRemoveAll_Int,
    tags: [.validation, .api, .Array],
    setUpFunction: { blackHole(inputArray_Int) },
    tearDownFunction: { inputArray_Int = nil }
  )
]

let size_Int = 1_000_000
let size_Class = 100_000

var inputArray_Int: [Int]! = {
  var a: [Int] = []
  a.reserveCapacity(size_Int)
  for i in 0 ..< size_Int {
    a.append(i)
  }
  return a
}()

var inputArray_Class: [Slow]! = {
  var a: [Slow] = []
  a.reserveCapacity(size_Class)
  for i in 0 ..< size_Class {
    a.append(Slow(num: i))
  }
  return a
}()

class Slow {
  public var num: Int
    
  init(num: Int) {
    self.num = num
  }
}


@inline(never)
func verifyCapacity<T>(_ new: [T], orig: [T]) -> Bool {
    return new.capacity == orig.capacity
}

@inline(never)
func removeAll<T>(_ arr: [T]) -> [T] {
  var copy = arr
  copy.removeAll(keepingCapacity: true)
  return copy
}

@inline(never)
func copyItem<T>(_ item: T) -> T {
  return item
}

@inline(never)
func run_ArrayRemoveAll_Class(_ n: Int) {
  for _ in 1...n {
    let copy = removeAll(inputArray_Class)
    check(verifyCapacity(copy, orig: inputArray_Class))
  }
}

@inline(never)
func run_ArrayRemoveAll_Int(_ n: Int) {
  for _ in 1...n {
    let copy = removeAll(inputArray_Int)
    check(verifyCapacity(copy, orig: inputArray_Int))
  }
}
