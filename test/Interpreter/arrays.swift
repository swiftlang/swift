// RUN: %swift -I %S/.. -i %s | FileCheck %s

// Create a new array
var a = new Int[10]
foreach i in 0..10 { a[i] = i }
a.replPrint()
// CHECK: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

// Copy a slice to another slice
a[1..3] = a[5..7]
a.replPrint()
// CHECK: [0, 5, 6, 3, 4, 5, 6, 7, 8, 9]

// Copy a slice to another slice with overlap
a[4..8] = a[6..10]
a.replPrint()
// CHECK: [0, 5, 6, 3, 6, 7, 8, 9, 8, 9]

// Create another array and copy in a slice
var b = new Int[10]
b[3..7] = a[5..9]
b.replPrint()
// CHECK: [0, 0, 0, 7, 8, 9, 8, 0, 0, 0]

