// RUN: %swift -I %S/.. %s -i | FileCheck %s

// From http://rosettacode.org/wiki/Factorial
func factorial(x:int) -> int {
  if (x == 0) { return 1 }
  return x * factorial(x-1)
}

// From http://rosettacode.org/wiki/Towers_of_Hanoi
// FIXME: this can be simplified with printf(), once it exists.
func TowersOfHanoi(ndisks:int, from:int, to:int, via:int) -> void {
  if (ndisks == 1) {
    print("Move disk from pole ")
    print(from)
    print(" to pole ")
    print(to)
    print("\n")
  }
  else {
    TowersOfHanoi(ndisks-1, from, via, to);
    TowersOfHanoi(1, from, to, via);
    TowersOfHanoi(ndisks-1, via, to, from);
  }
}

// Driver code.
print("Factorial of 10 = ")
print(factorial(10))
print("\n\nTowers of Hanoi, 4 disks\n")
TowersOfHanoi(4, 1, 2, 3);

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
