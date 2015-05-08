// RUN: %target-run-simple-swift | FileCheck %s

// Foreach loops

print("start foreach")
for i in 0...6 where i & 1 == 0 {
  print(i)
}
print("end foreach")

// CHECK-LABEL: start foreach
// CHECK-NEXT: 0
// CHECK-NEXT: 2
// CHECK-NEXT: 4
// CHECK-NEXT: 6
// CHECK-NEXT: end


