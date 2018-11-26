// RUN: %target-swift-emit-silgen -module-name Swift -enable-sil-ownership -parse-stdlib %s | %FileCheck %s

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
// CHECK-LABEL: sil hidden @$ss9arg_tuple1x1yySi_SftF
// CHECK: bb0([[X:%[0-9]+]] : @trivial $Int, [[Y:%[0-9]+]] : @trivial $Float):

arg_tuple(x: i, y: f)

func arg_deep_tuples(x: Int, y: (Float, UnicodeScalar)) {}
// CHECK-LABEL: sil hidden @$ss15arg_deep_tuples1x1yySi_Sf_ScttF
// CHECK: bb0([[X:%[0-9]+]] : @trivial $Int, [[Y_0:%[0-9]+]] : @trivial $Float, [[Y_1:%[0-9]+]] : @trivial $UnicodeScalar):

arg_deep_tuples(x:i, y:(f, c))
var unnamed_subtuple = (f, c)
arg_deep_tuples(x:i, y: unnamed_subtuple)
// rdar://problem/12985801 -- tuple conversion confuses named fields
// of a subtuple with those of outer tuple
var named_subtuple = (x:f, y:c)
arg_deep_tuples(x:i, y: named_subtuple)

func arg_deep_tuples_2(x: Int, _: (y: Float, z: UnicodeScalar)) {}
// CHECK-LABEL: sil hidden @$ss17arg_deep_tuples_21x_ySi_Sf1y_Sc1zttF
// CHECK: bb0([[X:%[0-9]+]] : @trivial $Int, [[Y:%[0-9]+]] : @trivial $Float, [[Z:%[0-9]+]] : @trivial $UnicodeScalar):

arg_deep_tuples_2(x: i, (f, c))
arg_deep_tuples_2(x: i, unnamed_subtuple)

// FIXME rdar://problem/12985103 -- tuples don't convert recursively, so
// #var x = (1, (2.0, '3')); arg_deep_tuples_2(x)# doesn't type check
//arg_deep_tuples_2(deep_named_tuple)

func arg_default_tuple(x x: Int = i, y: Float = f) {}
// CHECK-LABEL: sil hidden @$ss17arg_default_tuple1x1yySi_SftF
// CHECK: bb0([[X:%[0-9]+]] : @trivial $Int, [[Y:%[0-9]+]] : @trivial $Float):

arg_default_tuple()
arg_default_tuple(x:i)
arg_default_tuple(y:f)
arg_default_tuple(x:i, y:f)


func variadic_arg_1(_ x: Int...) {}
// CHECK-LABEL: sil hidden @$ss14variadic_arg_1{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[X:%[0-9]+]] : @trivial $Array<Int>):

variadic_arg_1()
variadic_arg_1(i)
variadic_arg_1(i, i, i)


func variadic_arg_2(_ x: Int, _ y: Float...) {}
// CHECK-LABEL: sil hidden @$ss14variadic_arg_2{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[X:%[0-9]+]] : @trivial $Int, [[Y:%[0-9]+]] : @trivial $Array<Float>):

variadic_arg_2(i)
variadic_arg_2(i, f)
variadic_arg_2(i, f, f, f)

func variadic_arg_3(_ y: Float..., x: Int) {}
// CHECK-LABEL: sil hidden @$ss14variadic_arg_3{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[Y:%[0-9]+]] : @trivial $Array<Float>, [[X:%[0-9]+]] : @trivial $Int):

variadic_arg_3(x: i)
variadic_arg_3(f, x: i)
variadic_arg_3(f, f, f, x: i)

protocol Runcible {}

extension Int : Runcible {}

func variadic_address_only_arg(_ x: Runcible...) {}

variadic_address_only_arg(i)
