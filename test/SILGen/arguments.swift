// RUN: %target-swift-frontend -module-name Swift -parse-stdlib -emit-silgen %s | %FileCheck %s

struct Int {}
struct Float {}
struct UnicodeScalar {}

enum Never {}

// Minimal implementation to support varargs.
struct Array<T> { }

func _allocateUninitializedArray<T>(_: Builtin.Word)
  -> (Array<T>, Builtin.RawPointer) {
  Builtin.int_trap()
}

func _deallocateUninitializedArray<T>(_: Array<T>) {}

var i:Int, f:Float, c:UnicodeScalar

func arg_tuple(x: Int, y: Float) {}
// CHECK-LABEL: sil hidden @_T0s9arg_tupleySi1x_Sf1ytF
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Float):

arg_tuple(x: i, y: f)

func arg_deep_tuples(x: Int, y: (Float, UnicodeScalar)) {}
// CHECK-LABEL: sil hidden @_T0s15arg_deep_tuplesySi1x_Sf_Sct1ytF
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y_0:%[0-9]+]] : $Float, [[Y_1:%[0-9]+]] : $UnicodeScalar):

arg_deep_tuples(x:i, y:(f, c))
var unnamed_subtuple = (f, c)
arg_deep_tuples(x:i, y: unnamed_subtuple)
// rdar://problem/12985801 -- tuple conversion confuses named fields
// of a subtuple with those of outer tuple
var named_subtuple = (x:f, y:c)
arg_deep_tuples(x:i, y: named_subtuple)

func arg_deep_tuples_2(x: Int, _: (y: Float, z: UnicodeScalar)) {}
// CHECK-LABEL: sil hidden @_T0s17arg_deep_tuples_2ySi1x_Sf1y_Sc1zttF
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Float, [[Z:%[0-9]+]] : $UnicodeScalar):

arg_deep_tuples_2(x: i, (f, c))
arg_deep_tuples_2(x: i, unnamed_subtuple)

// FIXME rdar://problem/12985103 -- tuples don't convert recursively, so
// #var x = (1, (2.0, '3')); arg_deep_tuples_2(x)# doesn't type check
//arg_deep_tuples_2(deep_named_tuple)

func arg_default_tuple(x x: Int = i, y: Float = f) {}
// CHECK-LABEL: sil hidden @_T0s17arg_default_tupleySi1x_Sf1ytF
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Float):

arg_default_tuple()
arg_default_tuple(x:i)
arg_default_tuple(y:f)
arg_default_tuple(x:i, y:f)


func variadic_arg_1(_ x: Int...) {}
// CHECK-LABEL: sil hidden @_T0s14variadic_arg_1{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[X:%[0-9]+]] : $Array<Int>):

variadic_arg_1()
variadic_arg_1(i)
variadic_arg_1(i, i, i)


func variadic_arg_2(_ x: Int, _ y: Float...) {}
// CHECK-LABEL: sil hidden @_T0s14variadic_arg_2{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Array<Float>):

variadic_arg_2(i)
variadic_arg_2(i, f)
variadic_arg_2(i, f, f, f)

func variadic_arg_3(_ y: Float..., x: Int) {}
// CHECK-LABEL: sil hidden @_T0s14variadic_arg_3{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[Y:%[0-9]+]] : $Array<Float>, [[X:%[0-9]+]] : $Int):

variadic_arg_3(x: i)
variadic_arg_3(f, x: i)
variadic_arg_3(f, f, f, x: i)

protocol Runcible {}

extension Int : Runcible {}

func variadic_address_only_arg(_ x: Runcible...) {}

variadic_address_only_arg(i)
