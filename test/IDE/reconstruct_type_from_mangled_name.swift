// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | FileCheck %s -implicit-check-not="cannot reconstruct"


class Mystruct1 {
// CHECK: reconstructed decl from usr for 'Mystruct1' is 'class Mystruct1'
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
