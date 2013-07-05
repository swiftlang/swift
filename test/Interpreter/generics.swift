// RUN: %swift -i %s | FileCheck %s

struct BigStruct { var a,b,c,d,e,f,g,h:Int }

// FIXME: missing symbol for Object destructor?
//class SomeClass : Object { }

func id<T>(x:T) -> T {
  return x
}

var int = id(1)
var bigStruct = id(BigStruct(1,2,3,4,5,6,7,8))
//var someClass = SomeClass()
//var someClass2 = id(someClass)

func println(bs:BigStruct) {
  // FIXME: typechecker is too slow to handle this as an interpolated literal
  print("BigStruct(")
  print(bs.a)
  print(", ")
  print(bs.b)
  print(", ")
  print(bs.c)
  print(", ")
  print(bs.d)
  print(", ")
  print(bs.e)
  print(", ")
  print(bs.f)
  print(", ")
  print(bs.g)
  print(", ")
  print(bs.h)
  println(")")
}

// CHECK: 1
println(int)
// CHECK: BigStruct(1, 2, 3, 4, 5, 6, 7, 8)
println(bigStruct)

// FIXME: missing symbol for Object destructor?
// C/HECK: true
//println(someClass === someClass2)
