// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

// CHECK-DAG: !DILocalVariable(name: "arg", arg: 1,{{.*}} line: [[@LINE+1]]
func a(_ arg : Int64)
{
// CHECK-DAG: !DILocalVariable(name: "local",{{.*}} line: [[@LINE+1]]
  var local = arg
}

// CHECK-DAG: !DILocalVariable(name: "a", arg: 1,{{.*}} line: [[@LINE+3]]
// CHECK-DAG: !DILocalVariable(name: "b", arg: 2,{{.*}} line: [[@LINE+2]]
// CHECK-DAG: !DILocalVariable(name: "c", arg: 3,{{.*}} line: [[@LINE+1]]
func many(_ a: Int64, b: (Int64, Int64), c: Int64) -> Int64 {
// CHECK-DAG: !DILocalVariable(name: "i1",{{.*}} line: [[@LINE+1]]
  var i1 = a
// CHECK-DAG: !DILocalVariable(name: "i2",{{.*}} line: [[@LINE+2]]
// CHECK-DAG: !DILocalVariable(name: "i3",{{.*}} line: [[@LINE+1]]
  var (i2, i3) : (Int64, Int64) = b
// CHECK-DAG: !DILocalVariable(name: "i4",{{.*}} line: [[@LINE+1]]
  var i4 = c
  return i1+i2+i3+i4
}

class A {
  var member : Int64
// CHECK-DAG: !DILocalVariable(name: "a", arg: 1,{{.*}} line: [[@LINE+1]]
  init(a: Int64) { member = a }

// CHECK-DAG: !DILocalVariable(name: "offset", arg: 1,{{.*}} line: [[@LINE+2]]
// CHECK-DAG: !DILocalVariable(name: "self", arg: 2,{{.*}} line: [[@LINE+1]]
  func getValuePlus(_ offset: Int64) -> Int64 {
// CHECK-DAG: !DILocalVariable(name: "a",{{.*}} line: [[@LINE+1]]
    var a = member
    return a+offset
  }

// CHECK-DAG: !DILocalVariable(name: "factor", arg: 1,{{.*}} line: [[@LINE+3]]
// CHECK-DAG: !DILocalVariable(name: "offset", arg: 2,{{.*}} line: [[@LINE+2]]
// CHECK-DAG: !DILocalVariable(name: "self", arg: 3,{{.*}} line: [[@LINE+1]]
  func getValueTimesPlus(_ factor: Int64, offset: Int64) -> Int64 {
// CHECK-DAG: !DILocalVariable(name: "a",{{.*}} line: [[@LINE+1]]
    var a = member
// CHECK-DAG: !DILocalVariable(name: "f",{{.*}} line: [[@LINE+1]]
    var f = factor
    return a*f+offset
  }

// CHECK: !DILocalVariable(name: "self", arg: 1,{{.*}} line: [[@LINE+1]]
  deinit {
    markUsed(member)
  }

}

// CHECK: !DILocalVariable(name: "x", arg: 1,{{.*}} line: [[@LINE+2]]
// CHECK: !DILocalVariable(name: "y", arg: 2,{{.*}} line: [[@LINE+1]]
func tuple(_ x: Int64, y: (Int64, Float, String)) -> Int64 {
  return x+y.0
}
