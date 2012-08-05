// RUN: %swift -i %s | FileCheck %s

// Create a new array
var arr = new Int[10]
for i in 0..10 { arr[i] = i % 7 + 2 }
arr.replPrint()
print('\n')
// CHECK: [2, 3, 4, 5, 6, 7, 8, 2, 3, 4]

// min and max element
println(minElement(arr))
// CHECK-NEXT: 2
println(maxElement(arr))
// CHECK-NEXT: 8

// min and max element of a slice
println(minElement(arr[1..5]))
// CHECK-NEXT: 3
println(maxElement(arr[1..5]))
// CHECK-NEXT: 6


