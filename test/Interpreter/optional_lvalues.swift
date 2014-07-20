// RUN: %target-run-simple-swift | FileCheck %s

var x: Int! = 0
x! = 2
println(x) // CHECK: 2
x!++
println(x) // CHECK-NEXT: 3

var sequences = ["fibonacci": [1, 1, 2, 3, 0]]
println(sequences) // CHECK-NEXT: [fibonacci: [1, 1, 2, 3, 0]]
sequences["fibonacci"]![4] = 5
println(sequences) // CHECK-NEXT: [fibonacci: [1, 1, 2, 3, 5]]
sequences["fibonacci"]!.append(8)
println(sequences) // CHECK-NEXT: [fibonacci: [1, 1, 2, 3, 5, 8]]
