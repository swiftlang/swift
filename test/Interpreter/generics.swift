// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

struct BigStruct { var a,b,c,d,e,f,g,h:Int }

// FIXME: missing symbol for Object destructor?
//class SomeClass : Object { }

func id<T>(_ x: T) -> T {
  return x
}

var int = id(1)
var bigStruct = id(BigStruct(a: 1, b: 2,c: 3, d: 4, e: 5, f: 6, g: 7, h: 8))
//var someClass = SomeClass()
//var someClass2 = id(someClass)

func print(_ bs: BigStruct) {
  // FIXME: typechecker is too slow to handle this as an interpolated literal
  print("BigStruct(", terminator: "")
  print(bs.a, terminator: "")
  print(", ", terminator: "")
  print(bs.b, terminator: "")
  print(", ", terminator: "")
  print(bs.c, terminator: "")
  print(", ", terminator: "")
  print(bs.d, terminator: "")
  print(", ", terminator: "")
  print(bs.e, terminator: "")
  print(", ", terminator: "")
  print(bs.f, terminator: "")
  print(", ", terminator: "")
  print(bs.g, terminator: "")
  print(", ", terminator: "")
  print(bs.h, terminator: "")
  print(")")
}

// CHECK: 1
print(int)
// CHECK: BigStruct(1, 2, 3, 4, 5, 6, 7, 8)
print(bigStruct)

// FIXME: missing symbol for Object destructor?
// C/HECK: true
//print(someClass === someClass2)


//===----
// Check overload resolution of generic functions.
//===----

protocol P1 {}
protocol P2 : P1 {}
protocol P3 : P2 {}

struct S1 : P1 {}
struct S2 : P2 {}
struct S3 : P3 {}

func foo1<T : P1>(_ x: T) { print("P1") }
func foo1<T : P2>(_ x: T) { print("P2") }
func foo1<T : P3>(_ x: T) { print("P3") }

func foo2<T : P1>(_ x: T) { print("P1") }
func foo2<T : P2>(_ x: T) { print("P2") }

func foo3<T : P1>(_ x: T) { print("P1") }
func foo3<T : P3>(_ x: T) { print("P3") }

func foo4<T : P3, U : P1>(_ x: T, _ y: U) { print("P3, P1") }
func foo4<T : P3, U : P3>(_ x: T, _ y: U) { print("P3, P3") }

func checkOverloadResolution() {
  print("overload resolution:")
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

class Base {
    var v = 0
    required init() {}
    func map() {
        v = 1
    }
}

class D1 : Base {
    required init() {}
    override func map() {
        v = 2
    }
}

func parse<T:Base>() -> T {
    let inst = T()
    inst.map()
    return inst
}

var m : D1 = parse()

print(m.v)
// CHECK: 2

// rdar://problem/21770225 - infinite recursion with metadata
// instantiation when associated type contains Self

struct MySlice<T : MyIndexableType> : MySequenceType {}
struct MyMutableSlice<T : MyMutableCollectionType> : MySequenceType {}

protocol MySequenceType {}
protocol MyIndexableType {}

protocol MyCollectionType : MySequenceType, MyIndexableType {
  associatedtype SubSequence = MySlice<Self>
}
protocol MyMutableCollectionType : MyCollectionType {
  associatedtype SubSequence = MyMutableSlice<Self>
}

struct S : MyMutableCollectionType {}

// CHECK: MyMutableSlice<S>()
print(S.SubSequence())

// CHECK: MyMutableSlice<S>
print(S.SubSequence.self)
