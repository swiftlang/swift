// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-run %t/a.out.optimized | %FileCheck %s
// REQUIRES: executable_test

func anyToIntPoint(_ x: Any) -> (x: Int, y: Int) {
  return x as! (x: Int, y: Int)
}

func anyToIntPointOpt(_ x: Any) -> (x: Int, y: Int)? {
  return x as? (x: Int, y: Int)
}

func anyToInt2(_ x: Any) -> (Int, Int) {
  return x as! (Int, Int)
}

// Labels can be added/removed.
print("Label add/remove")          // CHECK: Label add/remove
print(anyToIntPoint((x: 1, y: 2))) // CHECK-NEXT: (1, 2)
print(anyToIntPoint((3, 4)))       // CHECK-NEXT: (3, 4)
print(anyToIntPoint((x: 5, 6)))    // CHECK-NEXT: (5, 6)
print(anyToInt2((1, 2)))           // CHECK-NEXT: (1, 2)
print(anyToInt2((x: 3, y: 4)))     // CHECK-NEXT: (3, 4)
print(anyToInt2((x: 5, 6)))        // CHECK-NEXT: (5, 6)

// Labels cannot be wrong.
print("Wrong labels")                 // CHECK: Wrong labels
print(anyToIntPointOpt((x: 1, z: 2))) // CHECK-NEXT: nil
print(anyToIntPointOpt((x: 1, y: 2))) // CHECK-NEXT: Optional((1, 2))
