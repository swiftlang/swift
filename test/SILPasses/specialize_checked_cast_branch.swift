// RUN: %swift -emit-sil -O3 -sil-inline-threshold 0 %s -o - | FileCheck %s

class C {}
class D : C {}
class E {}

var b : UInt8 = 0
var c = C()
var d = D()
var e = E()
var f : UInt64 = 0
var o : AnyObject = c

////////////////////////
// Arch to Arch Casts //
////////////////////////

func ArchetypeToArchetypeCast<T1, T2>(`t1 : T1, `t2 : T2) -> T2 {
  if let x = t1 as T2 {
    return x
  }
  fatal("??? Profit?")
}

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C_S0____TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out C, @in C, @in C) -> () {
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: c, t2: c)

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_TTSVSs5UInt8_S____TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out UInt8, @in UInt8, @in UInt8) -> () {
// CHECK: function_ref @_TTSVSs5UInt8___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__ : $@thin (@out Optional<UInt8>, @in UInt8) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: b, t2: b)

// x -> y where x,y are not classes and x is a different type from y.
// CHECK-LABEL: sil shared @_TTSVSs5UInt8_VSs6UInt64___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out UInt64, @in UInt8, @in UInt64) -> () {
// CHECK: function_ref @_TTSVSs6UInt64___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<UInt64>) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: b, t2: f)

// x -> y where x is not a class but y is.
// CHECK-LABEL: sil shared @_TTSVSs5UInt8_C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out C, @in UInt8, @in C) -> () {
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<C>) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: b, t2: c)

// y -> x where x is a class but y is not.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C_VSs5UInt8___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out UInt8, @in C, @in UInt8) -> () {
// CHECK: function_ref @_TTSVSs5UInt8___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<UInt8>) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: c, t2: b)

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C_CS_1D___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out D, @in C, @in D) -> () {
// CHECK: unconditional_checked_cast downcast {{%[0-9]+}}#1 : $*C to $*D
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1D___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__ : $@thin (@out Optional<D>, @in D) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: c, t2: d)

// y -> x where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1D_CS_1C___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out C, @in D, @in C) -> () {
// CHECK: upcast {{%[0-9]+}}#1 : $*D to $*C
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: d, t2: c)

// x -> y where x and y are unrelated.
// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C_CS_1E___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@thin (@out E, @in C, @in E) -> () {
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1E___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<E>) -> ()
// CHECK: cond_br {{%[0-9]+}}, bb1, bb2
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: c, t2: e)

///////////////////////////
// Archetype To Concrete //
///////////////////////////

func ArchetypeToConcreteCastUInt8<T>(`t : T) -> UInt8 {
  if let x = t as UInt8 {
    return x
  }
  fatal("??? Profit?")
}

func ArchetypeToConcreteCastC<T>(`t : T) -> C {
  if let x = t as C {
    return x
  }
  fatal("??? Profit?")
}

func ArchetypeToConcreteCastD<T>(`t : T) -> D {
  if let x = t as D {
    return x
  }
  fatal("??? Profit?")
}

func ArchetypeToConcreteCastE<T>(`t : T) -> E {
  if let x = t as E {
    return x
  }
  fatal("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSVSs5UInt8___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8U__FT1tQ__VSs5UInt8 : $@thin (@in UInt8) -> UInt8 {
// CHECK: bb0
// CHECK: function_ref @_TTSVSs5UInt8___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb1
ArchetypeToConcreteCastUInt8(t: b)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8U__FT1tQ__VSs5UInt8 : $@thin (@in C) -> UInt8 {
// CHECK: bb0
// CHECK: function_ref @_TTSVSs5UInt8___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<UInt8>) -> ()
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb1
ArchetypeToConcreteCastUInt8(t: c)

// CHECK-LABEL: sil shared @_TTSVSs6UInt64___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8U__FT1tQ__VSs5UInt8 : $@thin (@in UInt64) -> UInt8 {
// CHECK: bb0
// CHECK: function_ref @_TTSVSs5UInt8___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__ : $@thin (@out Optional<UInt8>) -> ()
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb1
ArchetypeToConcreteCastUInt8(t: f)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCU__FT1tQ__CS_1C : $@thin (@in C) -> @owned C {
// CHECK: bb0
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb1
ArchetypeToConcreteCastC(t: c)

// CHECK-LABEL: sil shared @_TTSVSs5UInt8___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCU__FT1tQ__CS_1C : $@thin (@in UInt8) -> @owned C {
// CHECK: bb0
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb1
ArchetypeToConcreteCastC(t: b)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCU__FT1tQ__CS_1C : $@thin (@in D) -> @owned C {
// CHECK: bb0
// CHECK: upcast
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb1
ArchetypeToConcreteCastC(t: d)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1E___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCU__FT1tQ__CS_1C : $@thin (@in E) -> @owned C {
// CHECK: bb0
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb1
ArchetypeToConcreteCastC(t: e)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastEU__FT1tQ__CS_1E : $@thin (@in C) -> @owned E {
// CHECK: bb0
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1E___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb1
ArchetypeToConcreteCastE(t: c)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastDU__FT1tQ__CS_1D : $@thin (@in C) -> @owned D {
// CHECK: bb0
// CHECK: unconditional_checked_cast downcast
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1D___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: bb0
ArchetypeToConcreteCastD(t: c)

///////////////////////////
// Concrete To Archetype //
///////////////////////////

func ConcreteToArchetypeCastUInt8<T>(`t: UInt8, `t2: T) -> T {
  if let x = t as T {
    return x
  }
  fatal("??? Profit?")
}
func ConcreteToArchetypeCastC<T>(`t: C, `t2: T) -> T {
  if let x = t as T {
    return x
  }
  fatal("??? Profit?")
}
func ConcreteToArchetypeCastD<T>(`t: D, `t2: T) -> T {
  if let x = t as T {
    return x
  }
  fatal("??? Profit?") 
}

// CHECK-LABEL: sil shared @_TTSVSs5UInt8___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@thin (@out UInt8, UInt8, @in UInt8) -> () {
// CHECK: bb0
// CHECK: function_ref @_TTSVSs5UInt8___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK: bb1
ConcreteToArchetypeCastUInt8(t: b, t2: b)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@thin (@out C, UInt8, @in C) -> () {
// CHECK: bb0
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NOT: checked_cast_br concrete_to_archetype
// CHECK: bb1
ConcreteToArchetypeCastUInt8(t: b, t2: c)

// CHECK-LABEL: sil shared @_TTSVSs6UInt64___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@thin (@out UInt64, UInt8, @in UInt64) -> () {
// CHECK: bb0
// CHECK: function_ref @_TTSVSs6UInt64___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NOT: checked_cast_br concrete_to_archetype
// CHECK: bb1
ConcreteToArchetypeCastUInt8(t: b, t2: f)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCU__FT1tCS_1C2t2Q__Q_ : $@thin (@out C, @owned C, @in C) -> () {
// CHECK: bb0
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br concrete_to_archetype
// CHECK: bb1
ConcreteToArchetypeCastC(t: c, t2: c)

// CHECK-LABEL: sil shared @_TTSVSs5UInt8___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCU__FT1tCS_1C2t2Q__Q_ : $@thin (@out UInt8, @owned C, @in UInt8) -> () {
// CHECK: bb0
// CHECK: function_ref @_TTSVSs5UInt8___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NOT: checked_cast_br concrete_to_archetype
// CHECK: bb1
ConcreteToArchetypeCastC(t: c, t2: b)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCU__FT1tCS_1C2t2Q__Q_ : $@thin (@out D, @owned C, @in D) -> () {
// CHECK: bb0
// CHECK: unconditional_checked_cast downcast
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1D___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br concrete_to_archetype
// CHECK: bb1
ConcreteToArchetypeCastC(t: c, t2: d)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1E___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCU__FT1tCS_1C2t2Q__Q_ : $@thin (@out E, @owned C, @in E) -> () {
// CHECK: bb0
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1E___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NOT: checked_cast_br concrete_to_archetype
// CHECK: bb1
ConcreteToArchetypeCastC(t: c, t2: e)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastDU__FT1tCS_1D2t2Q__Q_ : $@thin (@out C, @owned D, @in C) -> () {
// CHECK: bb0
// CHECK: upcast
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br concrete_to_archetype
ConcreteToArchetypeCastD(t: d, t2: c)

////////////////////////
// Super To Archetype //
////////////////////////

func SuperToArchetypeCastC<T>(`c : C, `t : T) -> T {
  if let x = c as T {
    return x
  }
  fatal("??? Profit?") 
}

func SuperToArchetypeCastD<T>(`d : D, `t : T) -> T {
  if let x = d as T {
    return x
  }
  fatal("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch21SuperToArchetypeCastCU__FT1cCS_1C1tQ__Q_ : $@thin (@out C, @owned C, @in C) -> () {
// CHECK: bb0
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br super_to_archetype
// CHECK: bb1
SuperToArchetypeCastC(c: c, t: c)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch21SuperToArchetypeCastCU__FT1cCS_1C1tQ__Q_ : $@thin (@out D, @owned C, @in D) -> () {
// CHECK: bb0
// CHECK: unconditional_checked_cast downcast
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1D___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br super_to_archetype
// CHECK: bb1
SuperToArchetypeCastC(c: c, t: d)

// CHECK-LABEL: sil shared @_TTSVSs5UInt8___TF30specialize_checked_cast_branch21SuperToArchetypeCastCU__FT1cCS_1C1tQ__Q_ : $@thin (@out UInt8, @owned C, @in UInt8) -> () {
// CHECK: bb0
// CHECK: function_ref @_TTSVSs5UInt8___TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK-NOT: checked_cast_br super_to_archetype
// CHECK: bb1
SuperToArchetypeCastC(c: c, t: b)

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch21SuperToArchetypeCastDU__FT1dCS_1D1tQ__Q_ : $@thin (@out C, @owned D, @in C) -> () {
// CHECK: bb0
// CHECK: upcast
// CHECK: function_ref @_TTSC30specialize_checked_cast_branch1C___TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NOT: checked_cast_br super_to_archetype
// CHECK: bb1
SuperToArchetypeCastD(d: d, t: c)

//////////////////////////////
// Existential To Archetype //
//////////////////////////////

func ExistentialToArchetypeCast<T>(`o : AnyObject, `t : T) -> T {
  if let x = o as T {
    return x
  }
  fatal("??? Profit?") 
}

// CHECK-LABEL: sil shared @_TTSC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch26ExistentialToArchetypeCastU__FT1oPSs9AnyObject_1tQ__Q_ : $@thin (@out C, @owned AnyObject, @in C) -> () {
// CHECK: bb0
// CHECK: checked_cast_br existential_to_concrete
// CHECK: bb1
ExistentialToArchetypeCast(o: o, t: c)

// CHECK-LABEL: sil shared @_TTSVSs5UInt8___TF30specialize_checked_cast_branch26ExistentialToArchetypeCastU__FT1oPSs9AnyObject_1tQ__Q_ : $@thin (@out UInt8, @owned AnyObject, @in UInt8) -> () {
// CHECK: bb0
// CHECK: checked_cast_br existential_to_concrete
// CHECK: bb1
ExistentialToArchetypeCast(o: o, t: b)
