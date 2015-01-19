// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

// CHECK-DAG: [ DW_TAG_arg_variable ] [arg] [line [[@LINE+1]]]
func a(arg : Int)
{
// CHECK-DAG: [ DW_TAG_auto_variable ] [local] [line [[@LINE+1]]]
  var local = arg
}

// WTF, you ask? This field is ArgNo << 24 | LineNo!
// CHECK-DAG: \0016{{.*}} [ DW_TAG_arg_variable ] [a] [line [[@LINE+3]]]
// CHECK-DAG: \0033{{.*}} [ DW_TAG_arg_variable ] [b] [line [[@LINE+2]]]
// CHECK-DAG: \0067{{.*}} [ DW_TAG_arg_variable ] [c] [line [[@LINE+1]]]
func many(a: Int, b: (Int, Int), c: Int) -> Int {
// CHECK-DAG: [ DW_TAG_auto_variable ] [i1] [line [[@LINE+1]]]
  var i1 = a
// CHECK-DAG: [ DW_TAG_auto_variable ] [i2] [line [[@LINE+2]]]
// CHECK-DAG: [ DW_TAG_auto_variable ] [i3] [line [[@LINE+1]]]
  var (i2: Int, i3: Int) = b
// CHECK-DAG: [ DW_TAG_auto_variable ] [i4] [line [[@LINE+1]]]
  var i4 = c
  return i1+i2+i3+i4
}

class A {
  var member : Int
// CHECK-DAG: \0016{{.*}} [ DW_TAG_arg_variable ] [a] [line [[@LINE+1]]]
  init(a: Int) { member = a }

// CHECK-DAG: \0016{{.*}}[ DW_TAG_arg_variable ] [offset] [line [[@LINE+2]]]
// CHECK-DAG: [ DW_TAG_arg_variable ] [self] [line [[@LINE+1]]]
  func getValuePlus(offset: Int) -> Int {
// CHECK-DAG: [ DW_TAG_auto_variable ] [a] [line [[@LINE+1]]]
    var a = member
    return a+offset
  }

// CHECK-DAG: \0016{{.*}}[ DW_TAG_arg_variable ] [factor] [line [[@LINE+3]]]
// CHECK-DAG: \0033{{.*}}[ DW_TAG_arg_variable ] [offset] [line [[@LINE+2]]]
// CHECK-DAG: [ DW_TAG_arg_variable ] [self] [line [[@LINE+1]]]
  func getValueTimesPlus(factor: Int, offset: Int) -> Int {
// CHECK-DAG: [ DW_TAG_auto_variable ] [a] [line [[@LINE+1]]]
    var a = member
// CHECK-DAG: [ DW_TAG_auto_variable ] [f] [line [[@LINE+1]]]
    var f = factor
    return a*f+offset
  }

// CHECK: \0016{{.*}}[ DW_TAG_arg_variable ] [self] [line 50]
  deinit {
    println(member)
  }

}

// Curried functions have their arguments backwards.
// CHECK: \0016{{.*}} [ DW_TAG_arg_variable ] [b] [line [[@LINE+2]]]
// CHECK: \0033{{.*}} [ DW_TAG_arg_variable ] [a] [line [[@LINE+1]]]
func uncurry (a: Int) (b: Int) -> (Int, Int) {
  return (a, b)
}

// CHECK: \0016{{.*}} [ DW_TAG_arg_variable ] [x] [line [[@LINE+2]]]
// CHECK: \0033{{.*}} [ DW_TAG_arg_variable ] [y] [line [[@LINE+1]]]
func tuple(x: Int, y: (Int, Float, String)) -> Int {
  return x+y.0;
}
