// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test


@inline(never)
func testit(_ input: [[Int]]) -> [[Int]] {
  var result : [[Int]] = []
  for inputElementArray in input {
    var mutableElementArray = inputElementArray
    for _ in 0..<2 {
      // The COW-check for 'mutableElementArray' must not be hoisted out of this loop.
      for i in 0..<1 {
        mutableElementArray[i] += 1
      }
      result.append(mutableElementArray)
    }
  }
  return result
}

// CHECK: {{\[\[1\], \[2\]\]}}
print(testit([[0]]))
