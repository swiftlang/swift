// RUN: %target-run-simple-swift | FileCheck %s

// From http://rosettacode.org/wiki/Factorial
func factorial(x: Int) -> Int {
  if (x == 0) { return 1 }
  return x * factorial(x-1)
}

// From http://rosettacode.org/wiki/Towers_of_Hanoi
func TowersOfHanoi(ndisks: Int, from: Int, to: Int, via: Int) -> Void {
  if (ndisks == 1) {
    print("Move disk from pole \(from) to pole \(to)\n")
  }
  else {
    TowersOfHanoi(ndisks-1, from: from, to: via, via: to);
    TowersOfHanoi(1, from: from, to: to, via: via);
    TowersOfHanoi(ndisks-1, from: via, to: to, via: from);
  }
}

// Driver code.
print("Factorial of 10 = \(factorial(10))\n\n")
print("Towers of Hanoi, 4 disks\n")
TowersOfHanoi(4, from: 1, to: 2, via: 3)

// CHECK: Factorial of 10 = 3628800
// CHECK: Towers of Hanoi, 4 disks
// CHECK-NEXT: Move disk from pole 1 to pole 3
// CHECK-NEXT: Move disk from pole 1 to pole 2
// CHECK-NEXT: Move disk from pole 3 to pole 2
// CHECK-NEXT: Move disk from pole 1 to pole 3
// CHECK-NEXT: Move disk from pole 2 to pole 1
// CHECK-NEXT: Move disk from pole 2 to pole 3
// CHECK-NEXT: Move disk from pole 1 to pole 3
// CHECK-NEXT: Move disk from pole 1 to pole 2
// CHECK-NEXT: Move disk from pole 3 to pole 2
// CHECK-NEXT: Move disk from pole 3 to pole 1
// CHECK-NEXT: Move disk from pole 2 to pole 1
// CHECK-NEXT: Move disk from pole 3 to pole 2
// CHECK-NEXT: Move disk from pole 1 to pole 3
// CHECK-NEXT: Move disk from pole 1 to pole 2
// CHECK-NEXT: Move disk from pole 3 to pole 2
