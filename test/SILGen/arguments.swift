// RUN: %target-swift-frontend -module-name Swift -parse-stdlib -emit-silgen %s | FileCheck %s

struct Int {}
struct Float {}
struct UnicodeScalar {}

// Minimal implementation to support varargs.
struct Array<T> { }

func _allocateUninitializedArray<T>(_: Builtin.Word)
  -> (Array<T>, Builtin.RawPointer) {
  Builtin.int_trap()
}

func _deallocateUninitializedArray<T>(_: Array<T>) {}

var i:Int, f:Float, c:UnicodeScalar

func arg_tuple(#x: Int, y: Float) {}
// CHECK-LABEL: sil hidden @_TFSs9arg_tupleFT1xSi1ySf_T_
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Float):

arg_tuple(x: i, y: f)

func arg_deep_tuples(#x: Int, y: (Float, UnicodeScalar)) {}
// CHECK-LABEL: sil hidden @_TFSs15arg_deep_tuplesFT1xSi1yTSfSc__T_
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y_0:%[0-9]+]] : $Float, [[Y_1:%[0-9]+]] : $UnicodeScalar):

arg_deep_tuples(x:i, y:(f, c))
var unnamed_subtuple = (f, c)
arg_deep_tuples(x: i, y: unnamed_subtuple)
// FIXME rdar://problem/12985801 -- tuple conversion confuses named fields
// of a subtuple with those of outer tuple
//var named_subtuple = (x:f, y:c)
//arg_deep_tuples(i, named_subtuple)

func arg_deep_tuples_2(#x: Int, (y: Float, z: UnicodeScalar)) {}
// CHECK-LABEL: sil hidden @_TFSs17arg_deep_tuples_2FT1xSiT1ySf1zSc__T_
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Float, [[Z:%[0-9]+]] : $UnicodeScalar):

arg_deep_tuples_2(x: i, (f, c))
arg_deep_tuples_2(x: i, unnamed_subtuple)

// FIXME rdar://problem/12985103 -- tuples don't convert recursively, so
// #var x = (1, (2.0, '3')); arg_deep_tuples_2(x)# doesn't type check
//arg_deep_tuples_2(deep_named_tuple)

func arg_default_tuple(#x: Int = i, y: Float = f) {}
// CHECK-LABEL: sil hidden @_TFSs17arg_default_tupleFT1xSi1ySf_T_
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Float):

arg_default_tuple()
arg_default_tuple(x:i)
arg_default_tuple(y:f)
arg_default_tuple(y:f, x:i)
arg_default_tuple(x:i, y:f)


func variadic_arg_1(x: Int...) {}
// CHECK-LABEL: sil hidden @_TFSs14variadic_arg_1
// CHECK: bb0([[X:%[0-9]+]] : $Array<Int>):

variadic_arg_1()
variadic_arg_1(i)
variadic_arg_1(i, i, i)


func variadic_arg_2(x: Int, _ y: Float...) {}
// CHECK-LABEL: sil hidden @_TFSs14variadic_arg_2
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Array<Float>):

variadic_arg_2(i)
variadic_arg_2(i, f)
variadic_arg_2(i, f, f, f)

protocol Runcible {}

extension Int : Runcible {}

func variadic_address_only_arg(x: Runcible...) {}

variadic_address_only_arg(i)
