// RUN: %target-swift-frontend  -emit-sil -O -sil-inline-threshold 0 %s -o - | %FileCheck %s

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
public func ArchetypeToArchetypeCast<T1, T2>(t1 : T1, t2 : T2) -> T2 {
  if let x = t1 as? T2 {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSg5Vs5UInt8___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8urFT1tx_Vs5UInt8 : $@convention(thin) (UInt8) -> UInt8 {
// CHECK: bb0
// CHECK: return %0 : $UInt8

// CHECK-LABEL: sil shared @_TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCurFT1tx_CS_1C : $@convention(thin) (@owned C) -> @owned C {
// CHECK: bb0
// CHECK: return %0 : $C

// CHECK-LABEL: sil shared @_TTSg5C30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCurFT1tx_CS_1C : $@convention(thin) (@owned D) -> @owned C {
// CHECK: bb0
// CHECK:  [[CAST:%.*]] = upcast %0 : $D to $C
// CHECK: return [[CAST]]

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastDurFT1tx_CS_1D : $@convention(thin) (@owned C) -> @owned D {
// CHECK: checked_cast_br %0 : $C to $D,
// CHECK: bb1([[T0:%.*]] : $D):
// CHECK: return [[T0]] : $D
// CHECK: bb2:
// CHECK: integer_literal $Builtin.Int1, -1
// CHECK: cond_fail
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1C_CS_1D___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastu0_rFT2t1x2t2q__q_ : $@convention(thin) (@owned C) -> @owned D
// CHECK: bb0
// CHECK:  checked_cast_br %0 : $C to $D, bb1, bb2
// CHECK: bb1(
// CHECK:   return
// CHECK: bb2
// CHECK:   integer_literal $Builtin.Int1, -1
// CHECK:   cond_fail
// CHECK:   unreachable

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1C_S0____TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastu0_rFT2t1x2t2q__q_ : $@convention(thin) (@owned C) -> @owned C {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: return %0 : $C

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5Vs5UInt8_S____TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastu0_rFT2t1x2t2q__q_ : $@convention(thin) (UInt8) -> UInt8 {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: return %0 : $UInt8

// x -> y where x,y are not classes and x is a different type from y.
// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5Vs5UInt8_Vs6UInt64___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastu0_rFT2t1x2t2q__q_ : $@convention(thin) () -> UInt64 {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: %0 = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail %0 : $Builtin.Int1
// CHECK: unreachable

// x -> y where x is not a class but y is.
// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5Vs5UInt8_C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastu0_rFT2t1x2t2q__q_ : $@convention(thin) () -> @owned C {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: %0 = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail %0 : $Builtin.Int1
// CHECK: unreachable

// y -> x where x is a class but y is not.
// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5C30specialize_checked_cast_branch1C_Vs5UInt8___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastu0_rFT2t1x2t2q__q_ : $@convention(thin) () -> UInt8 {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: %0 = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail %0 : $Builtin.Int1
// CHECK: unreachable

// y -> x where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1D_CS_1C___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastu0_rFT2t1x2t2q__q_ : $@convention(thin) (@owned D) -> @owned C {
// CHECK: [[T1:%.*]] = upcast %0 : $D to $C
// CHECK: return [[T1]] : $C

// x -> y where x and y are unrelated.
// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5C30specialize_checked_cast_branch1C_CS_1E___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastu0_rFT2t1x2t2q__q_ : $@convention(thin) () -> @owned E {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: %0 = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail %0 : $Builtin.Int1
// CHECK: unreachable


_ = ArchetypeToArchetypeCast(t1: c, t2: d)
_ = ArchetypeToArchetypeCast(t1: c, t2: c)
_ = ArchetypeToArchetypeCast(t1: b, t2: b)
_ = ArchetypeToArchetypeCast(t1: b, t2: f)
_ = ArchetypeToArchetypeCast(t1: b, t2: c)
_ = ArchetypeToArchetypeCast(t1: c, t2: b)
_ = ArchetypeToArchetypeCast(t1: d, t2: c)
_ = ArchetypeToArchetypeCast(t1: c, t2: e)

///////////////////////////
// Archetype To Concrete //
///////////////////////////

func ArchetypeToConcreteCastUInt8<T>(t : T) -> UInt8 {
  if let x = t as? UInt8 {
    return x
  }
  _preconditionFailure("??? Profit?")
}

func ArchetypeToConcreteCastC<T>(t : T) -> C {
  if let x = t as? C {
    return x
  }
  _preconditionFailure("??? Profit?")
}

func ArchetypeToConcreteCastD<T>(t : T) -> D {
  if let x = t as? D {
    return x
  }
  _preconditionFailure("??? Profit?")
}

func ArchetypeToConcreteCastE<T>(t : T) -> E {
  if let x = t as? E {
    return x
  }
  _preconditionFailure("??? Profit?")
}

_ = ArchetypeToConcreteCastUInt8(t: b)

// CHECK-LABEL: sil shared @_TTSf4d___TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8urFT1tx_Vs5UInt8 : $@convention(thin) () -> UInt8 {
// CHECK: bb0
// CHECK-NOT: checked_cast_br
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4d___TTSg5Vs6UInt64___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8urFT1tx_Vs5UInt8 : $@convention(thin) () -> UInt8 {
// CHECK-NEXT: bb0
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4d___TTSg5Vs5UInt8___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCurFT1tx_CS_1C : $@convention(thin) () -> @owned C {
// CHECK: bb0
// CHECK-NOT: checked_cast_br
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4d___TTSg5C30specialize_checked_cast_branch1E___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCurFT1tx_CS_1C : $@convention(thin) () -> @owned C {
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4d___TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastEurFT1tx_CS_1E : $@convention(thin) () -> @owned E {
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

_ = ArchetypeToConcreteCastUInt8(t: c)
_ = ArchetypeToConcreteCastUInt8(t: f)
_ = ArchetypeToConcreteCastC(t: c)
_ = ArchetypeToConcreteCastC(t: b)
_ = ArchetypeToConcreteCastC(t: d)
_ = ArchetypeToConcreteCastC(t: e)
_ = ArchetypeToConcreteCastD(t: c)
_ = ArchetypeToConcreteCastE(t: c)

///////////////////////////
// Concrete To Archetype //
///////////////////////////

func ConcreteToArchetypeCastUInt8<T>(t: UInt8, t2: T) -> T {
  if let x = t as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}
func ConcreteToArchetypeCastC<T>(t: C, t2: T) -> T {
  if let x = t as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}
func ConcreteToArchetypeCastD<T>(t: D, t2: T) -> T {
  if let x = t as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5Vs5UInt8___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8urFT1tVs5UInt82t2x_x : $@convention(thin) (UInt8) -> UInt8
// CHECK: bb0
// CHECK: return %0

// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8urFT1tVs5UInt82t2x_x : $@convention(thin) () -> @owned C
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5Vs6UInt64___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8urFT1tVs5UInt82t2x_x : $@convention(thin) () -> UInt64
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCurFT1tCS_1C2t2x_x : $@convention(thin) (@owned C) -> @owned C
// CHECK: bb0
// CHECK: return %0

// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5Vs5UInt8___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCurFT1tCS_1C2t2x_x : $@convention(thin) () -> UInt8
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCurFT1tCS_1C2t2x_x : $@convention(thin) (@owned C) -> @owned D
// CHECK: bb0
// CHECK:  checked_cast_br %0 : $C to $D
// CHECK: bb1

// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5C30specialize_checked_cast_branch1E___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCurFT1tCS_1C2t2x_x : $@convention(thin) () -> @owned E
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastDurFT1tCS_1D2t2x_x : $@convention(thin) (@owned D) -> @owned C
// CHECK: bb0
// CHECK:  [[T0:%.*]] = upcast %0 : $D to $C
// CHECK:  return [[T0]]

_ = ConcreteToArchetypeCastUInt8(t: b, t2: b)
_ = ConcreteToArchetypeCastUInt8(t: b, t2: c)
_ = ConcreteToArchetypeCastUInt8(t: b, t2: f)
_ = ConcreteToArchetypeCastC(t: c, t2: c)
_ = ConcreteToArchetypeCastC(t: c, t2: b)
_ = ConcreteToArchetypeCastC(t: c, t2: d)
_ = ConcreteToArchetypeCastC(t: c, t2: e)
_ = ConcreteToArchetypeCastD(t: d, t2: c)

////////////////////////
// Super To Archetype //
////////////////////////

func SuperToArchetypeCastC<T>(c : C, t : T) -> T {
  if let x = c as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}

func SuperToArchetypeCastD<T>(d : D, t : T) -> T {
  if let x = d as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch21SuperToArchetypeCastCurFT1cCS_1C1tx_x : $@convention(thin) (@owned C) -> @owned C
// CHECK: bb0
// CHECK: return %0 : $C

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch21SuperToArchetypeCastCurFT1cCS_1C1tx_x : $@convention(thin) (@owned C) -> @owned D
// CHECK: bb0
// CHECK:  checked_cast_br %0 : $C to $D
// CHECK: bb1

// CHECK-LABEL: sil shared @_TTSf4d_d___TTSg5Vs5UInt8___TF30specialize_checked_cast_branch21SuperToArchetypeCastCurFT1cCS_1C1tx_x : $@convention(thin) () -> UInt8
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch21SuperToArchetypeCastDurFT1dCS_1D1tx_x : $@convention(thin) (@owned D) -> @owned C
// CHECK: bb0
// CHECK:  [[T0:%.*]] = upcast %0 : $D to $C
// CHECK:  return [[T0]]

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch21SuperToArchetypeCastDurFT1dCS_1D1tx_x : $@convention(thin) (@owned D) -> @owned D
// CHECK: bb0
// CHECK: return %0 : $D

_ = SuperToArchetypeCastC(c: c, t: c)
_ = SuperToArchetypeCastC(c: c, t: d)
_ = SuperToArchetypeCastC(c: c, t: b)
_ = SuperToArchetypeCastD(d: d, t: c)
_ = SuperToArchetypeCastD(d: d, t: d)

//////////////////////////////
// Existential To Archetype //
//////////////////////////////

func ExistentialToArchetypeCast<T>(o : AnyObject, t : T) -> T {
  if let x = o as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch26ExistentialToArchetypeCasturFT1oPs9AnyObject_1tx_x : $@convention(thin) (@owned AnyObject) -> @owned C
// CHECK: bb0
// CHECK:  checked_cast_br %0 : $AnyObject to $C
// CHECK: bb1

// CHECK-LABEL: sil shared @_TTSf4g_d___TTSg5Vs5UInt8___TF30specialize_checked_cast_branch26ExistentialToArchetypeCasturFT1oPs9AnyObject_1tx_x : $@convention(thin) (@guaranteed AnyObject) -> UInt8
// CHECK: bb0
// CHECK:  checked_cast_addr_br take_always AnyObject in {{%.*}} : $*AnyObject to UInt8 in {{%.*}} : $*UInt8,
// CHECK: bb1

// CHECK-LABEL: sil shared @_TTSf4n_d___TTSg5Ps9AnyObject____TF30specialize_checked_cast_branch26ExistentialToArchetypeCasturFT1oPs9AnyObject_1tx_x : $@convention(thin) (@owned AnyObject) -> @owned AnyObject
// CHECK: bb0
// CHECK-NOT: checked_cast_br %
// CHECK: return %0 : $AnyObject
// CHECK-NOT: checked_cast_br %

_ = ExistentialToArchetypeCast(o: o, t: c)
_ = ExistentialToArchetypeCast(o: o, t: b)
_ = ExistentialToArchetypeCast(o: o, t: o)
