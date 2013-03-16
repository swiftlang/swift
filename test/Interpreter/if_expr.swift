// RUN: %swift -i %s | FileCheck %s

// CHECK: 1
// CHECK: 2
// CHECK: fizz
// CHECK: 4
// CHECK: buzz
// CHECK: fizz
// CHECK: 7
// CHECK: 8
// CHECK: fizz
// CHECK: buzz
for i in 1..11 {
  println(
    if i % 3 == 0
      then "fizz"
    else if i % 5 == 0
      then "buzz"
    else
      "\(i)")
}
