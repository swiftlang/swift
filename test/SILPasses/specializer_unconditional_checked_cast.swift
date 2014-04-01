// RUN: %swift -emit-sil -o - -O3 -sil-inline-threshold 1 -verify %s | FileCheck %s

func Convert<T1, T2>(t: T1, t2: T2) -> T2 {
  return (t as T2)!
}

class C {}
class D : C {}
class E {}

var b : UInt8 = 0
var c = C()
var d = D()
var e = E()

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSC38specializer_unconditional_checked_cast1C_S0____TF38specializer_unconditional_checked_cast7ConvertU___FT1tQ_2t2Q0__Q0_ : $@thin (@out C, @in C, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
Convert(c, c)

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_TTSVSs5UInt8_S____TF38specializer_unconditional_checked_cast7ConvertU___FT1tQ_2t2Q0__Q0_ : $@thin (@out UInt8, @in UInt8, @in UInt8) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
Convert(b, b)

// x -> y where x is not a class but y is.
// CHECK-LABEL: sil shared @_TTSVSs5UInt8_C38specializer_unconditional_checked_cast1C___TF38specializer_unconditional_checked_cast7ConvertU___FT1tQ_2t2Q0__Q0_ : $@thin (@out C, @in UInt8, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: [[TRAP:%[0-9]+]] = builtin_function_ref "int_trap" : $@thin @noreturn @callee_owned () -> ()
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK-NEXT: apply [[TRAP]]() : $@thin @noreturn @callee_owned () -> ()
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
Convert(b, c)

// y -> x where x is not a class but y is.
// CHECK-LABEL: sil shared @_TTSC38specializer_unconditional_checked_cast1C_VSs5UInt8___TF38specializer_unconditional_checked_cast7ConvertU___FT1tQ_2t2Q0__Q0_ : $@thin (@out UInt8, @in C, @in UInt8) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: [[TRAP:%[0-9]+]] = builtin_function_ref "int_trap" : $@thin @noreturn @callee_owned () -> ()
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK-NEXT: apply [[TRAP]]() : $@thin @noreturn @callee_owned () -> ()
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
Convert(c, b)

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSC38specializer_unconditional_checked_cast1C_CS_1D___TF38specializer_unconditional_checked_cast7ConvertU___FT1tQ_2t2Q0__Q0_ : $@thin (@out D, @in C, @in D) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: unconditional_checked_cast downcast {{%[0-9]+#1}} : $*C to $*D 
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
Convert(c, d)

// y -> x where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSC38specializer_unconditional_checked_cast1D_CS_1C___TF38specializer_unconditional_checked_cast7ConvertU___FT1tQ_2t2Q0__Q0_ : $@thin (@out C, @in D, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: upcast {{%[0-9]+#1}} : $*D to $*C
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
Convert(d, c)

// x -> y where x and y are unrelated.
// CHECK-LABEL: sil shared @_TTSC38specializer_unconditional_checked_cast1C_CS_1E___TF38specializer_unconditional_checked_cast7ConvertU___FT1tQ_2t2Q0__Q0_ : $@thin (@out E, @in C, @in E) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: [[TRAP:%[0-9]+]] = builtin_function_ref "int_trap" : $@thin @noreturn @callee_owned () -> ()
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK-NEXT: apply [[TRAP]]() : $@thin @noreturn @callee_owned () -> ()
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
Convert(c, e)
