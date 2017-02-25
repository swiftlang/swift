// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | %FileCheck %s -implicit-check-not="FAILURE"

struct Mystruct1 {
// CHECK: decl: struct Mystruct1
  func s1f1() -> Int { return 0 }
// CHECK: decl: func s1f1() -> Int
  var intField = 3
// CHECK: decl: var intField: Int
}
struct MyStruct2 {
// CHECK: decl: struct MyStruct2
  init() {}
// CHECK: decl: init()
  init(x: Int) {}
// CHECK: decl: init(x: Int)
  init(x: Int, y: Int) {}
// CHECK: decl: init(x: Int, y: Int)
}

class Myclass1 {
// CHECK: decl: class Myclass1
  var intField = 4
// CHECK: decl: var intField: Int
}

func f1() {
// CHECK: decl: func f1()
  var s1ins = Mystruct1() // Implicit ctor
// CHECK: decl: var s1ins: Mystruct1
// CHECK: dref: init() for 'Mystruct1'
  _ = Mystruct1(intField: 1) // Implicit ctor
// CHECK: dref: init(intField: Int)	for 'Mystruct1'

  s1ins.intField = 34
// CHECK: type: Mystruct1
// CHECK: type: Int

  var c1ins = Myclass1()
// CHECK: decl: var c1ins: Myclass1
// CHECK: dref: init()	for 'Myclass1'
// CHECK: type: Myclass1

  c1ins.intField = 3
// CHECK: type: Int

  s1ins.s1f1()
// CHECK: type: Mystruct1
// CHECK: type: (Mystruct1) -> () -> Int

  if let ifletf1 = Int?(1) {
// FIXME: lookup incorrect for if let binding.
// CHECK: decl: struct Int : {{.*}} for 'ifletf1' usr=s:14swift_ide_test2f1yyF7ifletf1L_Siv
  }
}

class Myclass2 {
// CHECK: decl: class Myclass2
  func f1() {
// CHECK: decl: func f1()

    var arr1 = [1, 2]
// CHECK: decl: var arr1: [Int]
// CHECK: type: Array<Int>

    arr1.append(1)
// FIXME: missing append()
// CHECK: dref: FAILURE	for 'append' usr=s:Sa6appendyxF
// CHECK: type: (@lvalue Array<Int>) -> (Int) -> ()

    var arr2 : [Mystruct1]
// CHECK: decl: var arr2: [Mystruct1]
// CHECK: type: Array<Mystruct1>

    arr2.append(Mystruct1())
// CHECK: type: (@lvalue Array<Mystruct1>) -> (Mystruct1) -> ()

    var arr3 : [Myclass1]
// CHECK: decl: var arr3: [Myclass1]
// CHECK: type: Array<Myclass1>

    arr3.append(Myclass1())
// CHECK: type: (@lvalue Array<Myclass1>) -> (Myclass1) -> ()

    _ = Myclass2.init()
// CHECK: dref: init()
  }
}

struct MyGenStruct1<T, U: ExpressibleByStringLiteral, V: Sequence> {
// CHECK: decl: struct MyGenStruct1<T, U, V> where U : ExpressibleByStringLiteral, V : Sequence
// FIXME: why are these references to the base type?
// FIXME: TypeReconstruction should support Node::Kind::GenericTypeParamDecl ('fp')
// CHECK: decl: FAILURE for 'T' usr=s:14swift_ide_test12MyGenStruct1V1Txmfp
// CHECK: decl: FAILURE for 'U' usr=s:14swift_ide_test12MyGenStruct1V1Uq_mfp
// CHECK: decl: FAILURE for 'V' usr=s:14swift_ide_test12MyGenStruct1V1Vq0_mfp

  let x: T
// CHECK: decl: let x: T
  let y: U
// CHECK: decl: let y: U
  let z: V
// CHECK: decl: let z: V

  func test000() {
    _ = x
// CHECK: type: T
    _ = y
// CHECK: type: U
    _ = z
// CHECK: type: V
  }
}

let genstruct1 = MyGenStruct1<Int, String, [Float]>(x: 1, y: "", z: [1.0])
// CHECK: decl: let genstruct1: MyGenStruct1<Int, String, [Float]>

func test001() {
// CHECK: decl: func test001()
  _ = genstruct1
// CHECK: type: MyGenStruct1<Int, String, Array<Float>>

  var genstruct2: MyGenStruct1<Int, String, [Int: Int]>
// CHECK: decl: var genstruct2: MyGenStruct1<Int, String, [Int : Int]>
  _ = genstruct2
// CHECK: type: MyGenStruct1<Int, String, Dictionary<Int, Int>>
  _ = genstruct2.x
// CHECK: type: Int
  _ = genstruct2.y
// CHECK: type: String
  _ = genstruct2.z
// CHECK: type: Dictionary<Int, Int>
}

protocol P1 {}
func foo1(p : P1) {}
// CHECK: decl: protocol P1  for 'P1' usr=s:14swift_ide_test2P1P
// CHECK: decl: func foo1(p: P1)  for 'foo1' usr=s:14swift_ide_test4foo1yAA2P1_p1p_tF
// CHECK: decl: let p: P1 for 'p' usr=s:14swift_ide_test4foo1yAA2P1_p1p_tFADL_AaC_pv
