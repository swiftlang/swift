// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | FileCheck %s -implicit-check-not="cannot reconstruct"

struct Mystruct1 {
// CHECK: reconstructed decl from usr for 'Mystruct1' is 'struct Mystruct1'
  func s1f1() -> Int { return 0 }
// CHECK: reconstructed decl from usr for 's1f1' is 'func s1f1() -> Int'
  var intField = 3
// CHECK: reconstructed decl from usr for 'intField' is 'var intField: Int'
}

class Myclass1 {
// CHECK: reconstructed decl from usr for 'Myclass1' is 'class Myclass1'
  var intField = 4
// CHECK: reconstructed decl from usr for 'intField' is 'var intField: Int'
}

func f1() {
// CHECK: reconstructed decl from usr for 'f1' is 'func f1()'
  var s1ins = Mystruct1()
// CHECK: reconstructed decl from usr for 's1ins' is 'var s1ins: Mystruct1'
// CHECK: reconstructed type from usr for 's1ins' is 'Mystruct1'

  s1ins.intField = 34
// CHECK: reconstructed type from usr for 'intField' is 'Int'

  var c1ins = Myclass1()
// CHECK: reconstructed decl from usr for 'c1ins' is 'var c1ins: Myclass1'
// CHECK: reconstructed type from usr for 'c1ins' is 'Myclass1'

  c1ins.intField = 3
// CHECK: reconstructed type from usr for 'intField' is 'Int'

  s1ins.s1f1()
// CHECK: reconstructed type from usr for 's1ins' is 'Mystruct1'
// CHECK: reconstructed type from usr for 's1f1' is 'Mystruct1 -> () -> Int'

  if let ifletf1 = Int?(1) {
// FIXME: lookup incorrect for if let binding.
// CHECK: reconstructed decl from usr for 'ifletf1' is 'struct Int : SignedInteger, Comparable, Equatable'
  }
}

class Myclass2 {
// CHECK: reconstructed decl from usr for 'Myclass2' is 'class Myclass2'
  func f1() {
// CHECK: reconstructed decl from usr for 'f1' is 'func f1()'

    var arr1 = [1, 2]
// CHECK: reconstructed decl from usr for 'arr1' is 'var arr1: [Int]'
// CHECK: reconstructed type from usr for 'arr1' is 'Array<Int>'

    arr1.append(1)
// CHECK: reconstructed type from usr for 'append' is '@lvalue Array<Int> -> Int -> ()'

    var arr2 : [Mystruct1]
// CHECK: reconstructed decl from usr for 'arr2' is 'var arr2: [Mystruct1]'
// CHECK: reconstructed type from usr for 'arr2' is 'Array<Mystruct1>'

    arr2.append(Mystruct1())
// CHECK: reconstructed type from usr for 'append' is '@lvalue Array<Mystruct1> -> Mystruct1 -> ()'

    var arr3 : [Myclass1]
// CHECK: reconstructed decl from usr for 'arr3' is 'var arr3: [Myclass1]'
// CHECK: reconstructed type from usr for 'arr3' is 'Array<Myclass1>'

    arr3.append(Myclass1())
// CHECK: reconstructed type from usr for 'append' is '@lvalue Array<Myclass1> -> Myclass1 -> ()'
  }
}

struct MyGenStruct1<T, U: StringLiteralConvertible, V: Sequence> {
// CHECK: reconstructed decl from usr for 'MyGenStruct1' is 'struct MyGenStruct1<T, U : StringLiteralConvertible, V : Sequence>'
// FIXME: why are these references to the base type?
// CHECK: reconstructed decl from usr for 'T' is 'struct MyGenStruct1
// CHECK: reconstructed decl from usr for 'U' is 'struct MyGenStruct1
// CHECK: reconstructed decl from usr for 'V' is 'struct MyGenStruct1

  let x: T
// CHECK: reconstructed decl from usr for 'x' is 'let x: T'
  let y: U
// CHECK: reconstructed decl from usr for 'y' is 'let y: U'
  let z: V
// CHECK: reconstructed decl from usr for 'z' is 'let z: V'

  func test000() {
    _ = x
// CHECK: reconstructed type from usr for 'x' is 'T'
    _ = y
// CHECK: reconstructed type from usr for 'y' is 'U'
    _ = z
// CHECK: reconstructed type from usr for 'z' is 'V'
  }
}

let genstruct1 = MyGenStruct1<Int, String, [Float]>(x: 1, y: "", z: [1.0])
// CHECK: reconstructed decl from usr for 'genstruct1' is 'let genstruct1: MyGenStruct1<Int, String, [Float]>'

func test001() {
// CHECK: reconstructed decl from usr for 'test001' is 'func test001()'
  _ = genstruct1
// CHECK: reconstructed type from usr for 'genstruct1' is 'MyGenStruct1<Int, String, Array<Float>>'

  var genstruct2: MyGenStruct1<Int, String, [Int: Int]>
// CHECK: reconstructed decl from usr for 'genstruct2' is 'var genstruct2: MyGenStruct1<Int, String, [Int : Int]>'
  _ = genstruct2
// CHECK: reconstructed type from usr for 'genstruct2' is 'MyGenStruct1<Int, String, Dictionary<Int, Int>>'
  _ = genstruct2.x
// CHECK: reconstructed type from usr for 'x' is 'Int'
  _ = genstruct2.y
// CHECK: reconstructed type from usr for 'y' is 'String'
  _ = genstruct2.z
// CHECK: reconstructed type from usr for 'z' is 'Dictionary<Int, Int>'
}
