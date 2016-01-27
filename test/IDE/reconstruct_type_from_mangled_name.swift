// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | FileCheck %s

class Mystruct1 {
  func s1f1() -> Int { return 0 }
  var intField = 3
}

class Myclass1 {
  var intField = 4
}

func f1() {
  var s1ins = Mystruct1()
  s1ins.intField = 34

// CHECK: reconstructed type from usr for 's1ins' is 'Mystruct1'
// CHECK: reconstructed type from usr for 'intField' is 'Int'
  var c1ins = Myclass1()
  c1ins.intField = 3

// CHECK: reconstructed type from usr for 'c1ins' is 'Myclass1'
// CHECK: reconstructed type from usr for 'intField' is 'Int'

  s1ins.s1f1()
// CHECK: reconstructed type from usr for 's1ins' is 'Mystruct1'
// CHECK: reconstructed type from usr for 's1f1' is 'Mystruct1 -> () -> Int'
}

class Myclass2 {
  func f1() {
    var arr1 = [1, 2]
    arr1.append(1)

// CHECK: reconstructed type from usr for 'arr1' is 'Array<Int>'
// CHECK: reconstructed type from usr for 'append' is '@lvalue Array<Int> -> Int -> ()'

    var arr2 : [Mystruct1]
    arr2.append(Mystruct1())
// CHECK: reconstructed type from usr for 'arr2' is 'Array<Mystruct1>'
// CHECK: reconstructed type from usr for 'append' is '@lvalue Array<Mystruct1> -> Mystruct1 -> ()'

    var arr3 : [Myclass1]
    arr3.append(Myclass1())
// CHECK: reconstructed type from usr for 'arr3' is 'Array<Myclass1>'
// CHECK: reconstructed type from usr for 'append' is '@lvalue Array<Myclass1> -> Myclass1 -> ()'
  }
}
