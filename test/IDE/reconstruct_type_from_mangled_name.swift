// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | %FileCheck %s -implicit-check-not="FAILURE"

struct Mystruct1 {
// CHECK: decl: struct Mystruct1
  func s1f1() -> Int { return 0 }
// CHECK: decl: func s1f1() -> Int
  var intField = 3
// CHECK: decl: @_hasInitialValue var intField: Int
// CHECK: decl: init(intField: Int)
// CHECK: decl: init()
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
// CHECK: decl: @_hasInitialValue var intField: Int
// CHECK: decl: init()
}

func f1() {
// CHECK: decl: func f1()
  var s1ins = Mystruct1() // Implicit ctor
// CHECK: decl: @_hasInitialValue var s1ins: Mystruct1
  _ = Mystruct1(intField: 1) // Implicit ctor

  s1ins.intField = 34
// CHECK: type: Mystruct1
// CHECK: type: Int

  var c1ins = Myclass1()
// CHECK: decl: @_hasInitialValue var c1ins: Myclass1
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
// CHECK: decl: @_hasInitialValue var arr1: [Int]
// CHECK: type: Array<Int>

    arr1.append(1)
// FIXME: missing append()
// CHECK: dref: FAILURE	for 'append' usr=s:Sa6appendyyxnF
// CHECK: type: (inout Array<Int>) -> (Int) -> ()

    var arr2 : [Mystruct1]
// CHECK: decl: var arr2: [Mystruct1]
// CHECK: type: Array<Mystruct1>

    arr2.append(Mystruct1())
// CHECK: type: (inout Array<Mystruct1>) -> (Mystruct1) -> ()

    var arr3 : [Myclass1]
// CHECK: decl: var arr3: [Myclass1]
// CHECK: type: Array<Myclass1>

    arr3.append(Myclass1())
// CHECK: type: (inout Array<Myclass1>) -> (Myclass1) -> ()

    _ = Myclass2.init()
// CHECK: dref: init()
  }
}

// CHECK: decl: enum MyEnum
enum MyEnum {
// FIXME
// CHECK: decl:   for 'ravioli'
  case ravioli
// CHECK: decl:   for 'pasta'
  case pasta

// CHECK: decl: func method() -> Int
  func method() -> Int { return 0 }

// CHECK: decl: func compare(_ other: MyEnum) -> Int
  func compare(_ other: MyEnum) -> Int {
    // CHECK: decl: let other: MyEnum
    return 0
  }

// CHECK: decl: mutating func mutatingMethod()
  mutating func mutatingMethod() {}
}

// CHECK: decl: func f2()
func f2() {
// CHECK: type: (MyEnum.Type) -> MyEnum
  var e = MyEnum.pasta

// CHECK: type: (MyEnum) -> () -> Int
  e.method()
// CHECK: (MyEnum) -> (MyEnum) -> Int
  e.compare(e)
// CHECK: (inout MyEnum) -> () -> ()
  e.mutatingMethod()
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

  // CHECK: decl: func takesT(_ t: T)
  func takesT(_ t: T) {
    // CHECK: decl: let t: T
  }
}

let genstruct1 = MyGenStruct1<Int, String, [Float]>(x: 1, y: "", z: [1.0])
// CHECK: decl: @_hasInitialValue let genstruct1: MyGenStruct1<Int, String, [Float]>

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

  genstruct2.takesT(123)
}

// CHECK: decl: protocol P1
protocol P1 {}

// CHECK: decl: func foo1(p: P1)
func foo1(p: P1) {
// CHECK: decl: let p: P1
// CHECK: type: (P1) -> ()
  foo1(p: p)
}

// CHECK: decl: protocol P2
protocol P2 {}

// CHECK: decl: func foo2(p: P1 & P2)
func foo2(p: P1 & P2) {
// CHECK: decl: let p: P1 & P2
  foo2(p: p)
}

// CHECK: func foo3(p: P1 & AnyObject)
func foo3(p: P1 & AnyObject) {
// CHECK: decl: let p: P1 & AnyObject
  foo3(p: p)
}

// CHECK: func foo4(p: Myclass1 & P1 & P2)
func foo4(p: P1 & P2 & Myclass1) {
// CHECK: decl: let p: Myclass1 & P1 & P2
  foo4(p: p)
}

func genericFunction<T : AnyObject>(t: T) {
// CHECK: decl: FAILURE for 'T' usr=s:14swift_ide_test15genericFunction1tyx_tRlzClF1TL_xmfp
  genericFunction(t: t)
}

// CHECK: decl: func takesInOut(fn: (inout Int) -> ())
// CHECK: decl: let fn: (inout Int) -> () for 'fn'
func takesInOut(fn: (inout Int) -> ()) {}

struct Outer {
  struct Inner {
    let x: Int
  }

  struct GenericInner<T> {
    // CHECK: decl: FAILURE for 'T' usr=s:14swift_ide_test5OuterV12GenericInnerV1Txmfp
    let t: T
  }
}

struct GenericOuter<T> {
  // CHECK: decl: FAILURE for 'T' usr=s:14swift_ide_test12GenericOuterV1Txmfp
  struct Inner {
    let t: T
    let x: Int
  }

  struct GenericInner<U> {
    // CHECK: decl: FAILURE for 'U' usr=s:14swift_ide_test12GenericOuterV0D5InnerV1Uqd__mfp
    let t: T
    let u: U
  }
}

// CHECK: decl: func takesGeneric(_ t: Outer.GenericInner<Int>)
func takesGeneric(_ t: Outer.GenericInner<Int>) {
  takesGeneric(t)
}

// CHECK: decl: func takesGeneric(_ t: GenericOuter<Int>.Inner)
func takesGeneric(_ t: GenericOuter<Int>.Inner) {
  takesGeneric(t)
}

// CHECK: decl: func takesGeneric(_ t: GenericOuter<Int>.GenericInner<String>)
func takesGeneric(_ t: GenericOuter<Int>.GenericInner<String>) {
  takesGeneric(t)
}

func hasLocalDecls() {
  func localFunction() {}

  // FIXME
  // CHECK: decl: FAILURE for 'LocalType'
  struct LocalType {
    // CHECK: FAILURE for 'localMethod'
    func localMethod() {}

    // CHECK: FAILURE for 'subscript(_:)'
    subscript(x: Int) { get {} set {} }

    // CHECK: decl: FAILURE for ''
    // CHECK: decl: FAILURE for ''
    // CHECK: decl: FAILURE for ''

  }

  // FIXME
  // CHECK: decl: FAILURE for 'LocalClass'
  class LocalClass {
    // CHECK: FAILURE for 'deinit'
    deinit {}

    // CHECK: decl: FAILURE for ''
  }

  // CHECK: decl: FAILURE for 'LocalAlias'
  typealias LocalAlias = LocalType
}

fileprivate struct VeryPrivateData {}

// CHECK: decl: fileprivate func privateFunction(_ d: VeryPrivateData) for 'privateFunction'
fileprivate func privateFunction(_ d: VeryPrivateData) {}

struct HasSubscript {
  // CHECK: decl: subscript(t: Int) -> Int { get set }
  subscript(_ t: Int) -> Int {
    // CHECK: decl: get	for '' usr=s:14swift_ide_test12HasSubscriptVyS2icig
    get {
      return t
    }
    // CHECK: decl: set	for '' usr=s:14swift_ide_test12HasSubscriptVyS2icis
    set {}
  }
}

// FIXME
// CHECK: decl: FAILURE	for 'T' usr=s:14swift_ide_test19HasGenericSubscriptV1Txmfp
struct HasGenericSubscript<T> {
  // CHECK: subscript<U>(t: T) -> U { get set }	for 'subscript(_:)' usr=s:14swift_ide_test19HasGenericSubscriptVyqd__xclui
  // FIXME
  // CHECK: decl: FAILURE	for 'U'
  // FIXME
  // CHECK: decl: FAILURE	for 't'
  subscript<U>(_ t: T) -> U {

    // CHECK: decl: get for '' usr=s:14swift_ide_test19HasGenericSubscriptVyqd__xcluig
    // FIXME
    // CHECK: dref: FAILURE	for 't'
    get {
      return t as! U
    }

    // CHECK: decl: set	for '' usr=s:14swift_ide_test19HasGenericSubscriptVyqd__xcluis
    set {}
  }
}

private
// CHECK: decl: private func patatino<T>(_ vers1: T, _ vers2: T) -> Bool where T : Comparable for
func patatino<T: Comparable>(_ vers1: T, _ vers2: T) -> Bool {
  // CHECK: decl: FAILURE   for 'T' usr=s:14swift_ide_test8patatino33_D7B956AE2D93947DFA67A1ECF93EF238LLySbx_xtSLRzlF1TL_xmfp decl
  // CHECK: decl: let vers1: T      for 'vers1' usr=s:14swift_ide_test8patatino33_D7B956AE2D93947DFA67A1ECF93EF238LLySbx_xtSLRzlF5vers1L_xvp
  // CHECK: decl: let vers2: T      for 'vers2' usr=s:14swift_ide_test8patatino33_D7B956AE2D93947DFA67A1ECF93EF238LLySbx_xtSLRzlF5vers2L_xvp
  return vers1 < vers2;
}
