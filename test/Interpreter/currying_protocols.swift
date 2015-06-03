// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

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
  print("\(t.scale(7)(y: 13))\n", appendNewline: false)
}

foo(S(offset: 4))
// CHECK: 95

let p : P = S(offset: 4)
print("\(p.scale(5)(y: 7))\n", appendNewline: false)
// CHECK: 39
