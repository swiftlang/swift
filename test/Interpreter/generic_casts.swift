// CHECK: %swift -i %s | FileCheck %s

func allToInt<T>(x:T) -> Int {
  return x as! Int
}

func anyToInt(x:protocol<>) -> Int {
  return x as! Int
}

protocol [class_protocol] Class {}

class C : Class {
  func print() { println("C!") }
}
class D : C {
  func print() { println("D!") }
}

func allClassesToC<T:Class>(x:T) -> C {
  return x as! C
}

func anyClassToC(x:protocol<Class>) -> C {
  return x as! C
}

println(allToInt(22)) // CHECK: 22
println(anyToInt(44)) // CHECK: 44
allClassesToC(C()).print() // CHECK: C!
allClassesToC(D()).print() // CHECK: D!
anyClassToC(C()).print() // CHECK: C!
anyClassToC(D()).print() // CHECK: D!
