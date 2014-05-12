// RUN: %target-run-simple-swift | FileCheck %s

// Create a new array
var a = new Int[10]
for i in 0...10 { a[i] = i }
replPrint(a)
// CHECK: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

// Create a new unsigned array
var u = new UInt64[10]
for i in 0...10 { u[i] = UInt64(i) }
replPrint(u)
// CHECK: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

// Copy a slice to another slice
a[1...3] = a[5...7]
replPrint(a)
// CHECK: [0, 5, 6, 3, 4, 5, 6, 7, 8, 9]

// Copy a slice to another slice with overlap
a[4...8] = a[6...10]
replPrint(a)
// CHECK: [0, 5, 6, 3, 6, 7, 8, 9, 8, 9]

// Create another array and copy in a slice
var b = new Int[10]
b[3..6] = a[5...9]
replPrint(b)
// CHECK: [0, 0, 0, 7, 8, 9, 8, 0, 0, 0]

// FIXME: This currently generates incorrect code!
// <rdar://problem/12110086>
/*
func replPrintSlice<T : ReplPrintable>(s : T[][]) {
  print('[')
  var first = true
  var total = 0
  for i in s {
    if first {
      first = false
    } else {
      print(", ")
    }
    replPrint(i)
    total = total + 1
    if total > 50 {
      print(" ...]")
      return
    }
  }
  print(']')
}

// Create a 2D array
var aa = new Int[10][]
for i in 0...10 { 
  var a = new Int[10]
  for j in 0...10 {
    a[j] = i*10 + j
  }
  aa[i] = a
}
replPrintSlice(aa)
// FIXME: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], [10, 11, 12, 13, 14, 15, 16, 17, 18, 19], [20, 21, 22, 23, 24, 25, 26, 27, 28, 29], [30, 31, 32, 33, 34, 35, 36, 37, 38, 39], [40, 41, 42, 43, 44, 45, 46, 47, 48, 49], [50...

// Copy slices in a 2D array
aa[1...3] = aa[2...4]
replPrintSlice(aa)
// FIXME: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], [20, 21, 22, 23, 24, 25, 26, 27, 28, 29], [30, 31, 32, 33, 34, 35, 36, 37, 38, 39], [30, 31, 32, 33, 34, 35, 36, 37, 38, 39], [40, 41, 42, 43, 44, 45, 46, 47, 48, 49], [50...], ...]
*/

// Verify that array literals don't leak. <rdar://problem/16536439>
class Canary {
  deinit { println("dead") }
}

println()

// CHECK: dead
// CHECK: dead
// CHECK: dead
{ [Canary(), Canary(), Canary()] }()


