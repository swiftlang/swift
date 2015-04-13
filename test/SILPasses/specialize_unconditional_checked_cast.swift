// RUN: %target-swift-frontend -emit-sil -o - -O -sil-inline-threshold 0 -disable-func-sig-opts -verify %s | FileCheck %s

//////////////////
// Declarations //
//////////////////

class C {}
class D : C {}
class E {}

var b : UInt8 = 0
var c = C()
var d = D()
var e = E()
var f : UInt64 = 0
var o : AnyObject = c

////////////////////////////
// Archetype To Archetype //
////////////////////////////

func ArchetypeToArchetype<T1, T2>(#t: T1, #t2: T2) -> T2 {
  return t as! T2
}

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C_S0____TF37specialize_unconditional_checked_cast20ArchetypeToArchetypeU___FT1tQ_2t2Q0__Q0_ : $@convention(thin) (@out C, @in C, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
ArchetypeToArchetype(t: c, t2: c)

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8_S____TF37specialize_unconditional_checked_cast20ArchetypeToArchetypeU___FT1tQ_2t2Q0__Q0_ : $@convention(thin) (@out UInt8, @in UInt8, @in UInt8) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
ArchetypeToArchetype(t: b, t2: b)

// x -> y where x is not a class but y is.
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8_C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast20ArchetypeToArchetypeU___FT1tQ_2t2Q0__Q0_ : $@convention(thin) (@out C, @in UInt8, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast_addr
// CHECK-NOT: unconditional_checked_cast_addr
// CHECK:     builtin "int_trap"
// CHECK-NOT: unconditional_checked_cast_addr
ArchetypeToArchetype(t: b, t2: c)

// y -> x where x is not a class but y is.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C_VSs5UInt8___TF37specialize_unconditional_checked_cast20ArchetypeToArchetypeU___FT1tQ_2t2Q0__Q0_ : $@convention(thin) (@out UInt8, @in C, @in UInt8) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: builtin "int_trap"
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
ArchetypeToArchetype(t: c, t2: b)

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C_CS_1D___TF37specialize_unconditional_checked_cast20ArchetypeToArchetypeU___FT1tQ_2t2Q0__Q0_ : $@convention(thin) (@out D, @in C, @in D) -> () {
// CHECK: unconditional_checked_cast_addr take_always C in %1 : $*C to D in
ArchetypeToArchetype(t: c, t2: d)

// y -> x where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1D_CS_1C___TF37specialize_unconditional_checked_cast20ArchetypeToArchetypeU___FT1tQ_2t2Q0__Q0_ : $@convention(thin) (@out C, @in D, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: upcast {{%[0-9]+}} : $D to $C
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
ArchetypeToArchetype(t: d, t2: c)

// x -> y where x and y are unrelated classes.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C_CS_1E___TF37specialize_unconditional_checked_cast20ArchetypeToArchetypeU___FT1tQ_2t2Q0__Q0_ : $@convention(thin) (@out E, @in C, @in E) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: builtin "int_trap"
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
ArchetypeToArchetype(t: c, t2: e)

// x -> y where x and y are unrelated non classes.
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8_VSs6UInt64___TF37specialize_unconditional_checked_cast20ArchetypeToArchetypeU___FT1tQ_2t2Q0__Q0_ : $@convention(thin) (@out UInt64, @in UInt8, @in UInt64) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK:      builtin "int_trap"
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
ArchetypeToArchetype(t: b, t2: f)

///////////////////////////
// Archetype To Concrete //
///////////////////////////

func ArchetypeToConcreteConvertUInt8<T>(#t: T) -> UInt8 {
  return t as! UInt8
}
func ArchetypeToConcreteConvertC<T>(#t: T) -> C {
  return t as! C
}
func ArchetypeToConcreteConvertD<T>(#t: T) -> D {
  return t as! D
}
func ArchetypeToConcreteConvertE<T>(#t: T) -> E {
  return t as! E
}

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8___TF37specialize_unconditional_checked_cast31ArchetypeToConcreteConvertUInt8U__FT1tQ__VSs5UInt8 : $@convention(thin) (@in UInt8) -> UInt8 {
// CHECK: bb0
// CHECK-NEXT: load
// CHECK-NEXT: return
ArchetypeToConcreteConvertUInt8(t: b)

// x -> y where y is a class but x is not.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast31ArchetypeToConcreteConvertUInt8U__FT1tQ__VSs5UInt8 : $@convention(thin) (@in C) -> UInt8 {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ArchetypeToConcreteConvertUInt8(t: c)

// x -> y where x,y are not classes and x is a different type from y.
// CHECK-LABEL: sil shared @_TTSg5VSs6UInt64___TF37specialize_unconditional_checked_cast31ArchetypeToConcreteConvertUInt8U__FT1tQ__VSs5UInt8 : $@convention(thin) (@in UInt64) -> UInt8 {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ArchetypeToConcreteConvertUInt8(t: f)

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertCU__FT1tQ__CS_1C : $@convention(thin) (@in C) -> @owned C {
// CHECK: bb0
// CHECK-NEXT: load
// CHECK-NEXT: return
ArchetypeToConcreteConvertC(t: c)

// x -> y where x is a class but y is not.
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertCU__FT1tQ__CS_1C : $@convention(thin) (@in UInt8) -> @owned C {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ArchetypeToConcreteConvertC(t: b)

// x -> y where x,y are classes and x is a super class of y.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1D___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertCU__FT1tQ__CS_1C : $@convention(thin) (@in D) -> @owned C {
// CHECK: bb0
// CHECK-NEXT: load
// CHECK-NEXT: upcast
// CHECK-NEXT: return
ArchetypeToConcreteConvertC(t: d)

// x -> y where x,y are classes, but x is unrelated to y.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1E___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertCU__FT1tQ__CS_1C : $@convention(thin) (@in E) -> @owned C {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ArchetypeToConcreteConvertC(t: e)

// x -> y where x,y are classes and x is a sub class of y.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertDU__FT1tQ__CS_1D : $@convention(thin) (@in C) -> @owned D {
// CHECK: bb0
// CHECK-NEXT: alloc_stack
// CHECK-NEXT: unconditional_checked_cast_addr take_always
// CHECK-NEXT: load
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: return
ArchetypeToConcreteConvertD(t: c)

// x -> y where x,y are classes, but y is unrelated to x. The idea is
// to make sure that the fact that y is concrete does not affect the
// result.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertEU__FT1tQ__CS_1E : $@convention(thin) (@in C) -> @owned E {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ArchetypeToConcreteConvertE(t: c)

///////////////////////////
// Concrete to Archetype //
///////////////////////////

func ConcreteToArchetypeConvertUInt8<T>(#t: UInt8, #t2: T) -> T {
  return t as! T
}
func ConcreteToArchetypeConvertC<T>(#t: C, #t2: T) -> T {
  return t as! T
}
func ConcreteToArchetypeConvertD<T>(#t: D, #t2: T) -> T {
  return t as! T
}

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8___TF37specialize_unconditional_checked_cast31ConcreteToArchetypeConvertUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@convention(thin) (@out UInt8, UInt8, @in UInt8) -> () {
// CHECK: bb0
// CHECK-NEXT: store
// CHECK-NEXT: tuple
// CHECK-NEXT: return
ConcreteToArchetypeConvertUInt8(t: b, t2: b)

// x -> y where x is not a class but y is a class.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast31ConcreteToArchetypeConvertUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@convention(thin) (@out C, UInt8, @in C) -> () {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ConcreteToArchetypeConvertUInt8(t: b, t2: c)

// x -> y where x,y are different non class types.
// CHECK-LABEL: sil shared @_TTSg5VSs6UInt64___TF37specialize_unconditional_checked_cast31ConcreteToArchetypeConvertUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@convention(thin) (@out UInt64, UInt8, @in UInt64) -> () {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ConcreteToArchetypeConvertUInt8(t: b, t2: f)

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertCU__FT1tCS_1C2t2Q__Q_ : $@convention(thin) (@out C, @owned C, @in C) -> () {
// CHECK: bb0
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return
ConcreteToArchetypeConvertC(t: c, t2: c)

// x -> y where x is a class but y is not.
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertCU__FT1tCS_1C2t2Q__Q_ : $@convention(thin) (@out UInt8, @owned C, @in UInt8) -> () {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ConcreteToArchetypeConvertC(t: c, t2: b)

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1D___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertCU__FT1tCS_1C2t2Q__Q_ : $@convention(thin) (@out D, @owned C, @in D) -> () {
// CHECK: bb0
// CHECK-NEXT: alloc_stack
// CHECK-NEXT: store
// CHECK-NEXT: strong_retain
// CHECK-NEXT: unconditional_checked_cast_addr take_always
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return
ConcreteToArchetypeConvertC(t: c, t2: d)

// x -> y where x and y are unrelated classes.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1E___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertCU__FT1tCS_1C2t2Q__Q_ : $@convention(thin) (@out E, @owned C, @in E) -> () {
// CHECK: bb0
// CHECK-NEXT: strong_retain
// CHECK-NEXT: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
ConcreteToArchetypeConvertC(t: c, t2: e)

// x -> y where x is a subclass of y.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertDU__FT1tCS_1D2t2Q__Q_ : $@convention(thin) (@out C, @owned D, @in C) -> () {
// CHECK: bb0
// CHECK-NEXT: upcast
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return
ConcreteToArchetypeConvertD(t: d, t2: c)

////////////////////////
// Super To Archetype //
////////////////////////

func SuperToArchetypeC<T>(#c : C, #t : T) -> T {
  return c as! T
}

func SuperToArchetypeD<T>(#d : D, #t : T) -> T {
  return d as! T
}

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast17SuperToArchetypeCU__FT1cCS_1C1tQ__Q_ : $@convention(thin) (@out C, @owned C, @in C) -> () {
// CHECK: bb0
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return
SuperToArchetypeC(c: c, t: c)

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1D___TF37specialize_unconditional_checked_cast17SuperToArchetypeCU__FT1cCS_1C1tQ__Q_ : $@convention(thin) (@out D, @owned C, @in D) -> () {
// CHECK: bb0
// CHECK: unconditional_checked_cast_addr take_always C in 
SuperToArchetypeC(c: c, t: d)

// x -> y where x is a class and y is not.
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8___TF37specialize_unconditional_checked_cast17SuperToArchetypeCU__FT1cCS_1C1tQ__Q_ : $@convention(thin) (@out UInt8, @owned C, @in UInt8) -> () {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
SuperToArchetypeC(c: c, t: b)

// *NOTE* The frontend is smart enough to turn this into an upcast. When this
// test is converted to SIL, this should be fixed appropriately.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast17SuperToArchetypeDU__FT1dCS_1D1tQ__Q_ : $@convention(thin) (@out C, @owned D, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast super_to_archetype
// CHECK: upcast
// CHECK-NOT: unconditional_checked_cast super_to_archetype
SuperToArchetypeD(d: d, t: c)

// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1D___TF37specialize_unconditional_checked_cast17SuperToArchetypeDU__FT1dCS_1D1tQ__Q_ : $@convention(thin) (@out D, @owned D, @in D) -> () {
// CHECK: bb0
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return
SuperToArchetypeD(d: d, t: d)

//////////////////////////////
// Existential To Archetype //
//////////////////////////////

func ExistentialToArchetype<T>(#o : AnyObject, #t : T) -> T {
  return o as! T
}

// AnyObject -> Class.
// CHECK-LABEL: sil shared @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast22ExistentialToArchetypeU__FT1oPSs9AnyObject_1tQ__Q_ : $@convention(thin) (@out C, @owned AnyObject, @in C) -> () {
// CHECK: unconditional_checked_cast_addr take_always AnyObject in {{%.*}} : $*AnyObject to C
ExistentialToArchetype(o: o, t: c)

// AnyObject -> Non Class (should always fail)
// CHECK-LABEL: sil shared @_TTSg5VSs5UInt8___TF37specialize_unconditional_checked_cast22ExistentialToArchetypeU__FT1oPSs9AnyObject_1tQ__Q_ : $@convention(thin) (@out UInt8, @owned AnyObject, @in UInt8) -> () {
// CHECK: builtin "int_trap"()
// CHECK: unreachable
// CHECK-NEXT: }
ExistentialToArchetype(o: o, t: b)

// AnyObject -> AnyObject
// CHECK-LABEL: sil shared @_TTSg5PSs9AnyObject____TF37specialize_unconditional_checked_cast22ExistentialToArchetypeU__FT1oPSs9AnyObject_1tQ__Q_ : $@convention(thin) (@out AnyObject, @owned AnyObject, @in AnyObject) -> () {
// CHECK: bb0
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return
ExistentialToArchetype(o: o, t: o)
