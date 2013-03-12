// RUN: %swift -i %s | FileCheck %s

func [infix] =~ (a:Int, b:Int) -> Bool { return a == b }

println("before")
// CHECK: before

for n in 0..5 {
  print("\(n) is ")
  switch n {
  case 0, 2, 4:
    println("even")
  default:
    println("just some number")
  case 1:
    println("the loneliest number that you'll ever do")
  }
}

// CHECK: 0 is even
// CHECK: 1 is the loneliest number that you'll ever do
// CHECK: 2 is even
// CHECK: 3 is just some number
// CHECK: 4 is even

println("after")
// CHECK: after

var x = 0
while true {
  switch ++x {
  case 4:
    break
  case 2:
    continue
  }
  println("\(x)")
}
// CHECK: 1
// CHECK: 3

