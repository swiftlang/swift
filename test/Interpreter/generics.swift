// RUN: %target-run-simple-swift | FileCheck %s

struct BigStruct { var a,b,c,d,e,f,g,h:Int }

// FIXME: missing symbol for Object destructor?
//class SomeClass : Object { }

func id<T>(x: T) -> T {
  return x
}

var int = id(1)
var bigStruct = id(BigStruct(a: 1, b: 2,c: 3, d: 4, e: 5, f: 6, g: 7, h: 8))
//var someClass = SomeClass()
//var someClass2 = id(someClass)

func println(bs: BigStruct) {
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


//===----
// Check overload resolution of generic functions.
//===----

protocol P1 {}
protocol P2 : P1 {}
protocol P3 : P2 {}

struct S1 : P1 {}
struct S2 : P2 {}
struct S3 : P3 {}

func foo1<T : P1>(x: T) { println("P1") }
func foo1<T : P2>(x: T) { println("P2") }
func foo1<T : P3>(x: T) { println("P3") }

func foo2<T : P1>(x: T) { println("P1") }
func foo2<T : P2>(x: T) { println("P2") }

func foo3<T : P1>(x: T) { println("P1") }
func foo3<T : P3>(x: T) { println("P3") }

func foo4<T : P3, U : P1>(x: T, y: U) { println("P3, P1") }
func foo4<T : P3, U : P3>(x: T, y: U) { println("P3, P3") }

func checkOverloadResolution() {
  println("overload resolution:")
  // CHECK-LABEL: overload resolution

  foo1(S1()) // CHECK-NEXT: P1
  foo1(S2()) // CHECK-NEXT: P2
  foo1(S3()) // CHECK-NEXT: P3

  foo2(S1()) // CHECK-NEXT: P1
  foo2(S2()) // CHECK-NEXT: P2
  foo2(S3()) // CHECK-NEXT: P2

  foo3(S1()) // CHECK-NEXT: P1
  foo3(S2()) // CHECK-NEXT: P1
  foo3(S3()) // CHECK-NEXT: P3

  foo4(S3(), S1()) // CHECK-NEXT: P3, P1
  foo4(S3(), S2()) // CHECK-NEXT: P3, P1
  foo4(S3(), S3()) // CHECK-NEXT: P3, P3
}
checkOverloadResolution()

class Base  {
    var v = 0
    required init() {}
    func map() {
        v = 1
    }
}

class D1 : Base {
    required  init() {}
    override func map() {
        v = 2
    }
}

func parse<T:Base>()->T {
    var inst = T()
    inst.map()
    return inst
}

var m : D1 = parse()

println(m.v)
// CHECK: 2

