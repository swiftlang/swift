// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// Foreach loops

print("foreach/where")
for i in 0...6 where i & 1 == 0 {
  print(i)
}

// CHECK-LABEL: foreach/where
// CHECK-NEXT: 0
// CHECK-NEXT: 2
// CHECK-NEXT: 4
// CHECK-NEXT: 6


let OptionalArr : [Int?] = [1, nil, 2, nil, 3, nil, nil, nil, 4, 5]

print("foreach/case")
for case let i? in OptionalArr {
  print(i)
}

// CHECK-LABEL: foreach/case
// CHECK-NEXT: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: 4
// CHECK-NEXT: 5

print("foreach/case/where")

for case let i? in OptionalArr where i != 3 {
  print(i)
}

// CHECK-LABEL: foreach/case/where
// CHECK-NEXT: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 4
// CHECK-NEXT: 5

print("guard top level")
guard let guardvalue = Optional(42) else { preconditionFailure() }
print(guardvalue)

// CHECK-LABEL: guard top level
// CHECK-NEXT: 42

print("done");
// CHECK-LABEL: done

