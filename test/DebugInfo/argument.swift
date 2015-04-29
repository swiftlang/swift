// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "arg", arg: 1,{{.*}} line: [[@LINE+1]]
func a(arg : Int)
{
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "local",{{.*}} line: [[@LINE+1]]
  var local = arg
}

// Why is c arg: 4?  Note that "b" shows up as both arg 2 and 3.
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "a", arg: 1,{{.*}} line: [[@LINE+3]]
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "b", arg: 2,{{.*}} line: [[@LINE+2]]
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "c", arg: 4,{{.*}} line: [[@LINE+1]]
func many(a: Int, b: (Int, Int), c: Int) -> Int {
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "i1",{{.*}} line: [[@LINE+1]]
  var i1 = a
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "i2",{{.*}} line: [[@LINE+2]]
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "i3",{{.*}} line: [[@LINE+1]]
  var (i2, i3) : (Int, Int) = b
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "i4",{{.*}} line: [[@LINE+1]]
  var i4 = c
  return i1+i2+i3+i4
}

class A {
  var member : Int
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "a", arg: 1,{{.*}} line: [[@LINE+1]]
  init(a: Int) { member = a }

// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "offset", arg: 1,{{.*}} line: [[@LINE+2]]
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "self", arg: 2,{{.*}} line: [[@LINE+1]]
  func getValuePlus(offset: Int) -> Int {
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "a",{{.*}} line: [[@LINE+1]]
    var a = member
    return a+offset
  }

// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "factor", arg: 1,{{.*}} line: [[@LINE+3]]
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "offset", arg: 2,{{.*}} line: [[@LINE+2]]
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "self", arg: 3,{{.*}} line: [[@LINE+1]]
  func getValueTimesPlus(factor: Int, offset: Int) -> Int {
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "a",{{.*}} line: [[@LINE+1]]
    var a = member
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "f",{{.*}} line: [[@LINE+1]]
    var f = factor
    return a*f+offset
  }

// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "self", arg: 1,{{.*}} line: [[@LINE+1]]
  deinit {
    println(member)
  }

}

// Curried functions have their arguments backwards.
// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "b", arg: 1,{{.*}} line: [[@LINE+2]]
// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "a", arg: 2,{{.*}} line: [[@LINE+1]]
func uncurry (a: Int) (b: Int) -> (Int, Int) {
  return (a, b)
}

// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "x", arg: 1,{{.*}} line: [[@LINE+2]]
// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "y", arg: 2,{{.*}} line: [[@LINE+1]]
func tuple(x: Int, y: (Int, Float, String)) -> Int {
  return x+y.0;
}
