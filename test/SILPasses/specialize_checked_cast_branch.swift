// RUN: %swift -emit-sil -O3 -sil-inline-threshold 0 %s -o - | FileCheck %s

class C {}
class D : C {}
class E {}

var b : UInt8 = 0
var c = C()
var d = D()
var e = E()
var f : UInt64 = 0

////////////////////////
// Arch to Arch Casts //
////////////////////////

func ArchToArchCast<T1, T2>(t1 : T1, t2 : T2) -> T2 {
  if let x = t1 as T2 {
    return x
  }
  fatal("??? Profit?")
}

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C_S0____TF30specialize_checked_cast_branch14ArchToArchCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out C, @in C, @in C) -> () {
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchToArchCast(c, c)

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_TTSVSs5UInt8_S____TF30specialize_checked_cast_branch14ArchToArchCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out UInt8, @in UInt8, @in UInt8) -> () {
// CHECK: function_ref @_TTSVSs5UInt8___TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__ : $@thin (@out Optional<UInt8>, @in UInt8) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchToArchCast(b, b)

// x -> y where x,y are not classes and x is a different type from y.
// CHECK-LABEL: sil shared @_TTSVSs5UInt8_VSs6UInt64___TF30specialize_checked_cast_branch14ArchToArchCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out UInt64, @in UInt8, @in UInt64) -> () {
// CHECK: function_ref @_TTSVSs6UInt64___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<UInt64>) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchToArchCast(b, f)

// x -> y where x is not a class but y is.
// CHECK-LABEL: sil shared @_TTSVSs5UInt8_C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch14ArchToArchCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out C, @in UInt8, @in C) -> () {
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<C>) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchToArchCast(b, c)

// y -> x where x is a class but y is not.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C_VSs5UInt8___TF30specialize_checked_cast_branch14ArchToArchCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out UInt8, @in C, @in UInt8) -> () {
// CHECK: function_ref @_TTSVSs5UInt8___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<UInt8>) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchToArchCast(c, b)

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C_CS_1D___TF30specialize_checked_cast_branch14ArchToArchCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out D, @in C, @in D) -> () {
// CHECK: unconditional_checked_cast downcast {{%[0-9]+}}#1 : $*C to $*D
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1D___TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__ : $@thin (@out Optional<D>, @in D) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchToArchCast(c, d)

// y -> x where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1D_CS_1C___TF30specialize_checked_cast_branch14ArchToArchCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out C, @in D, @in C) -> () {
// CHECK: upcast {{%[0-9]+}}#1 : $*D to $*C
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FT1vQ__GSqQ__
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchToArchCast(d, c)

// x -> y where x and y are unrelated.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C_CS_1E___TF30specialize_checked_cast_branch14ArchToArchCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out E, @in C, @in E) -> () {
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1E___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<E>) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchToArchCast(c, e)
