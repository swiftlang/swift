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

class Slow {
  public var num: Int

  init(num: Int) {
    self.num = num
  }
}

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

@inline(never)
func removeAll<T>(_ arr: [T]) -> [T] {
  var copy = arr
  copy.removeAll(keepingCapacity: true)
  return copy
}

@inline(never)
func run_ArrayRemoveAll_Class(_ n: Int) {
  var copy = removeAll(inputArray_Class);
  for _ in 1..<n {
    copy = removeAll(inputArray_Class)
  }
  check(copy.capacity == inputArray_Class.capacity)
}

@inline(never)
func run_ArrayRemoveAll_Int(_ n: Int) {
  var copy = removeAll(inputArray_Int);
  for _ in 1..<n {
    copy = removeAll(inputArray_Int)
  }
  check(copy.capacity == inputArray_Int.capacity)
}
