// RUN: %swift -dump-sil %s | FileCheck %s

var i:Int, f:Float, c:Char


func arg_tuple(x:Int, y:Float) {}
// CHECK: func_decl arg_tuple
// CHECK: bb0([[X:%[0-9]+]] : Int64, [[Y:%[0-9]+]] : Float32):

arg_tuple(i, f)
arg_tuple(x=i, y=f)
arg_tuple(y=f, x=i)
var unnamed_tuple = (i, f)
arg_tuple(unnamed_tuple)
var named_tuple = (x=i, y=f)
arg_tuple(named_tuple)
var shuffled_tuple = (y=f, x=i)
arg_tuple(shuffled_tuple)


func arg_deep_tuples(x:Int, y:(Float, Char)) {}
// CHECK: func_decl arg_deep_tuples
// CHECK: bb0([[X:%[0-9]+]] : Int64, [[Y_0:%[0-9]+]] : Float32, [[Y_1:%[0-9]+]] : Char):

arg_deep_tuples(i, (f, c))
arg_deep_tuples(x=i, y=(f, c))
arg_deep_tuples(y=(f, c), x=i)
var unnamed_subtuple = (f, c)
arg_deep_tuples(i, unnamed_subtuple)
// FIXME rdar://problem/12985801 -- tuple conversion confuses named fields
// of a subtuple with those of outer tuple
//var named_subtuple = (x=f, y=c)
//arg_deep_tuples(i, unnamed_subtuple)

var deep_unnamed_tuple = (i, (f, c))
arg_deep_tuples(deep_unnamed_tuple)

var deep_named_tuple = (x=i, y=(f, c))
arg_deep_tuples(deep_named_tuple)

var deep_shuffled_tuple = (y=(f, c), x=i)
arg_deep_tuples(deep_shuffled_tuple)


func arg_deep_tuples_2(x:Int, (y:Float, z:Char)) {}
// CHECK: func_decl arg_deep_tuples_2
// CHECK: bb0([[X:%[0-9]+]] : Int64, [[Y:%[0-9]+]] : Float32, [[Z:%[0-9]+]] : Char):

arg_deep_tuples_2(i, (f, c))
arg_deep_tuples_2(i, unnamed_subtuple)

// FIXME rdar://problem/12985103 -- tuples don't convert recursively, so
// `var x = (1, (2.0, '3')); arg_deep_tuples_2(x)` doesn't type check
//arg_deep_tuples_2(deep_named_tuple)

var deep_named_tuple_2 = (x=i, (y=f, z=c))
arg_deep_tuples_2(deep_named_tuple_2)
var deep_shuffled_tuple_2 = ((y=f, z=c), x=i)
arg_deep_tuples_2(deep_shuffled_tuple_2)


func arg_default_tuple(x:Int = 0, y:Float = 0.0) {}
// CHECK: func_decl arg_default_tuple
// CHECK: bb0([[X:%[0-9]+]] : Int64, [[Y:%[0-9]+]] : Float32):

arg_default_tuple()
arg_default_tuple(i)
arg_default_tuple(x=i)
arg_default_tuple(y=f)
arg_default_tuple(y=f, x=i)
arg_default_tuple(x=i, y=f)
