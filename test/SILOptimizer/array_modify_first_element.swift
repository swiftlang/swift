// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

// Check that a mutation of the first array element via the subscript `_modify`
// is not ignored by redundant load elimination. The array literal initializes
// the element with an `index_addr [projection]`, so the `_modify` must
// project the element address with `index_addr [projection]`, too.

@inline(never)
func testArray() {
  var a = [(1, 2), (3, 4)]
  a[0].1 += 1
  // CHECK: 3
  print(a[0].1)
}

@inline(never)
func testContiguousArray() {
  var a: ContiguousArray = [(1, 2), (3, 4)]
  a[0].1 += 1
  // CHECK: 3
  print(a[0].1)
}

@inline(never)
func testArraySlice() {
  var a = [(1, 2), (3, 4), (5, 6)][1...]
  a[1].1 += 1
  // CHECK: 5
  print(a[1].1)
}

@inline(never)
func testSwapAt() {
  var a = [1, 2]
  a.swapAt(0, 1)
  // CHECK: 2
  print(a[0])
  var c: ContiguousArray = [1, 2]
  c.swapAt(0, 1)
  // CHECK: 2
  print(c[0])
  var s = [0, 1, 2][1...]
  s.swapAt(1, 2)
  // CHECK: 2
  print(s[1])
}

testArray()
testContiguousArray()
testArraySlice()
testSwapAt()
