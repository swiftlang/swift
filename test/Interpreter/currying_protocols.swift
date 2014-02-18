// RUN: %target-run-simple-swift | FileCheck %s

protocol P {
  func scale(x : Int)(y : Int) -> Int
}

struct S : P  {
  var offset : Int
  func scale(x : Int)(y : Int) -> Int {
    return offset + x * y
  }
}

func foo<T : P>(t : T) {
  print("\(t.scale(7)(13))\n")
}

foo(S(4))
// CHECK: 95

val p : P = S(4)
print("\(p.scale(5)(7))\n")
// CHECK: 39
