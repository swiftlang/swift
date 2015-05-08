// RUN: %target-run-simple-swift | FileCheck %s

var x: Int! = 0
x! = 2
print(x) // CHECK: 2
x!++
print(x) // CHECK-NEXT: 3

var sequences = ["fibonacci": [1, 1, 2, 3, 0]]
print(sequences) // CHECK-NEXT: [fibonacci: [1, 1, 2, 3, 0]]
sequences["fibonacci"]![4] = 5
print(sequences) // CHECK-NEXT: [fibonacci: [1, 1, 2, 3, 5]]
sequences["fibonacci"]!.append(8)
print(sequences) // CHECK-NEXT: [fibonacci: [1, 1, 2, 3, 5, 8]]

func printAndReturn(x: Int) -> Int { print(x); return x }

print("optional binding") // CHECK-NEXT: optional binding
var y: Int? = nil
y? += printAndReturn(4)
print("done with binding test") // CHECK-NEXT: done with binding test
