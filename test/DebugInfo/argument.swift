// RUN: %swift -triple x86_64-apple-darwin13 %s -emit-llvm -g -o - | FileCheck %s

// CHECK: [ DW_TAG_arg_variable ] [arg] [line [[@LINE+1]]]
func a(arg : Int)
{
// CHECK: [ DW_TAG_auto_variable ] [local] [line [[@LINE+1]]]
  var local = arg
}

// WTF, you ask? This field is ArgNo << 24 | LineNo!
// CHECK: i32 16{{.*}} [ DW_TAG_arg_variable ] [a] [line [[@LINE+3]]]
// CHECK: i32 33{{.*}} [ DW_TAG_arg_variable ] [b] [line [[@LINE+2]]]
// CHECK: i32 67{{.*}} [ DW_TAG_arg_variable ] [c] [line [[@LINE+1]]]
func many(a: Int, b: (Int, Int), c: Int) -> Int {
// CHECK: [ DW_TAG_auto_variable ] [i1] [line [[@LINE+1]]]
  var i1 = a
// CHECK: [ DW_TAG_auto_variable ] [i2] [line [[@LINE+2]]]
// CHECK: [ DW_TAG_auto_variable ] [i3] [line [[@LINE+1]]]
  var (i2: Int, i3: Int) = b
// CHECK: [ DW_TAG_auto_variable ] [i4] [line [[@LINE+1]]]
  var i4 = c
  return i1+i2+i3+i4
}

class A {
  var member : Int
// CHECK: i32 33{{.*}} [ DW_TAG_arg_variable ] [self] [line 0]
// CHECK: i32 16{{.*}} [ DW_TAG_arg_variable ] [a] [line [[@LINE+1]]]
  init(a: Int) { member = a }
// CHECK: [ DW_TAG_arg_variable ] [offset] [line [[@LINE+1]]]
  func getValuePlus(offset: Int) -> Int {
// CHECK: [ DW_TAG_auto_variable ] [a] [line [[@LINE+1]]]
    var a = member
    return a+offset
  }

// CHECK: [ DW_TAG_arg_variable ] [offset] [line [[@LINE+1]]]
  func getValueTimesPlus(factor: Int, offset: Int) -> Int {
// CHECK: [ DW_TAG_auto_variable ] [a] [line [[@LINE+1]]]
    var a = member
// CHECK: [ DW_TAG_auto_variable ] [f] [line [[@LINE+1]]]
    var f = factor
    return a*f+offset
  }
}

// Curried functions have their arguments backwards.
// CHECK: i32 16{{.*}} [ DW_TAG_arg_variable ] [b] [line [[@LINE+2]]]
// CHECK: i32 33{{.*}} [ DW_TAG_arg_variable ] [a] [line [[@LINE+1]]]
func uncurry (a: Int) (b: Int) -> (Int, Int) {
  return (a, b)
}

// CHECK: i32 16{{.*}} [ DW_TAG_arg_variable ] [a] [line [[@LINE+3]]]
// CHECK: i32 33{{.*}} [ DW_TAG_arg_variable ] [b] [line [[@LINE+2]]]
// CHECK: i32 50{{.*}} [ DW_TAG_arg_variable ] [z] [line [[@LINE+1]]]
func aSelectorStyleWithArg (a: Int) andBerg(b: Int) andZerg(z: Int) -> Int {
  return (a+b)/z
}

// Having y and z show up as formal parameters is okay, because lldb
// does not rely on the formal parameters to build the function
// signature, it uses the mangled name instead. So what we represent
// in DWARF is closer to the lowered calling convention with all tuple
// arguments flattened.
//
// CHECK: i32 16{{.*}} [ DW_TAG_arg_variable ] [x] [line [[@LINE+3]]]
// CHECK: i32 33{{.*}} [ DW_TAG_arg_variable ] [y] [line [[@LINE+2]]]
// CHECK: i32 67{{.*}} [ DW_TAG_arg_variable ] [z] [line [[@LINE+1]]]
func pattern(x: Int, (y, _, z): (Int, Float, String)) -> Int {
  return x+y;
}

// CHECK: i32 16{{.*}} [ DW_TAG_arg_variable ] [x] [line [[@LINE+2]]]
// CHECK: i32 33{{.*}} [ DW_TAG_arg_variable ] [y] [line [[@LINE+1]]]
func tuple(x: Int, y: (Int, Float, String)) -> Int {
  return x+y.0;
}

// CHECK: i32 16{{.*}} [ DW_TAG_arg_variable ] [x1] [line [[@LINE+4]]]
// CHECK: i32 33{{.*}} [ DW_TAG_arg_variable ] [y1] [line [[@LINE+3]]]
// CHECK: i32 50{{.*}} [ DW_TAG_arg_variable ] [x2] [line [[@LINE+2]]]
// CHECK: i32 67{{.*}} [ DW_TAG_arg_variable ] [y2] [line [[@LINE+1]]]
func decomposePointTuples((x1: Int, y1: Int), (x2: Int, y2: Int)) {
}

// CHECK: i32 33{{.*}} [ DW_TAG_arg_variable ] [b] [line [[@LINE+3]]]
// CHECK: [ DW_TAG_auto_variable ] [local] [line [[@LINE+3]]]
// CHECK: i32 50{{.*}} [ DW_TAG_arg_variable ] [r] [line [[@LINE+1]]]
func snd( (_, b) : (Int, Int), r : @inout Int) {
  var local = b
  r = b
}
