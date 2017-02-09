// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests  -emit-sil -O -sil-inline-threshold 0 %s -o - | %FileCheck %s

class C {}
class D : C {}
class E {}

struct NotUInt8 { var value: UInt8 }
struct NotUInt64 { var value: UInt64 }

var b = NotUInt8(value: 0)
var c = C()
var d = D()
var e = E()
var f = NotUInt64(value: 0)
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

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8AA03NotI0Vx1t_tlFAD_Tg5 : $@convention(thin) (NotUInt8) -> NotUInt8 {
// CHECK: bb0
// CHECK: return %0 : $NotUInt8

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ArchetypeToConcreteCastCAA1CCx1t_tlFAD_Tg5 : $@convention(thin) (@owned C) -> @owned C {
// CHECK: bb0
// CHECK: return %0 : $C

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ArchetypeToConcreteCastCAA1CCx1t_tlFAA1DC_Tg5 : $@convention(thin) (@owned D) -> @owned C {
// CHECK: bb0
// CHECK:  [[CAST:%.*]] = upcast %0 : $D to $C
// CHECK: return [[CAST]]

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ArchetypeToConcreteCastDAA1DCx1t_tlFAA1CC_Tg5 : $@convention(thin) (@owned C) -> @owned D {
// CHECK: checked_cast_br %0 : $C to $D,
// CHECK: bb1([[T0:%.*]] : $D):
// CHECK: return [[T0]] : $D
// CHECK: bb2:
// CHECK: integer_literal $Builtin.Int1, -1
// CHECK: cond_fail
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch011ArchetypeToE4Castq_x2t1_q_2t2tr0_lFAA1CC_AA1DCTg5Tf4nd_n : $@convention(thin) (@owned C) -> @owned D
// CHECK: bb0
// CHECK:  checked_cast_br %0 : $C to $D, bb1, bb2
// CHECK: bb1(
// CHECK:   return
// CHECK: bb2
// CHECK:   integer_literal $Builtin.Int1, -1
// CHECK:   cond_fail
// CHECK:   unreachable

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch011ArchetypeToE4Castq_x2t1_q_2t2tr0_lFAA1CC_AFTg5Tf4nd_n : $@convention(thin) (@owned C) -> @owned C {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: return %0 : $C

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch011ArchetypeToE4Castq_x2t1_q_2t2tr0_lFAA8NotUInt8V_AFTg5Tf4nd_n : $@convention(thin) (NotUInt8) -> NotUInt8 {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: return %0 : $NotUInt8

// x -> y where x,y are not classes and x is a different type from y.
// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch011ArchetypeToE4Castq_x2t1_q_2t2tr0_lFAA8NotUInt8V_AA0J6UInt64VTg5Tf4dd_n : $@convention(thin) () -> NotUInt64 {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: %0 = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail %0 : $Builtin.Int1
// CHECK: unreachable

// x -> y where x is not a class but y is.
// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch011ArchetypeToE4Castq_x2t1_q_2t2tr0_lFAA8NotUInt8V_AA1CCTg5Tf4dd_n : $@convention(thin) () -> @owned C {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: %0 = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail %0 : $Builtin.Int1
// CHECK: unreachable

// y -> x where x is a class but y is not.
// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch011ArchetypeToE4Castq_x2t1_q_2t2tr0_lFAA1CC_AA8NotUInt8VTg5Tf4dd_n : $@convention(thin) () -> NotUInt8 {
// CHECK: bb0
// CHECK-NOT: bb1
// CHECK: %0 = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail %0 : $Builtin.Int1
// CHECK: unreachable

// y -> x where x is a super class of y.
// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch011ArchetypeToE4Castq_x2t1_q_2t2tr0_lFAA1DC_AA1CCTg5Tf4nd_n : $@convention(thin) (@owned D) -> @owned C {
// CHECK: [[T1:%.*]] = upcast %0 : $D to $C
// CHECK: return [[T1]] : $C

// x -> y where x and y are unrelated.
// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch011ArchetypeToE4Castq_x2t1_q_2t2tr0_lFAA1CC_AA1ECTg5Tf4dd_n : $@convention(thin) () -> @owned E {
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

func ArchetypeToConcreteCastUInt8<T>(t : T) -> NotUInt8 {
  if let x = t as? NotUInt8 {
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

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8AA03NotI0Vx1t_tlFAA1CC_Tg5Tf4d_n : $@convention(thin) () -> NotUInt8 {
// CHECK: bb0
// CHECK-NOT: checked_cast_br
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8AA03NotI0Vx1t_tlFAA0J6UInt64V_Tg5Tf4d_n : $@convention(thin) () -> NotUInt8 {
// CHECK-NEXT: bb0
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ArchetypeToConcreteCastCAA1CCx1t_tlFAA8NotUInt8V_Tg5Tf4d_n : $@convention(thin) () -> @owned C {
// CHECK: bb0
// CHECK-NOT: checked_cast_br
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ArchetypeToConcreteCastCAA1CCx1t_tlFAA1EC_Tg5Tf4d_n : $@convention(thin) () -> @owned C {
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ArchetypeToConcreteCastEAA1ECx1t_tlFAA1CC_Tg5Tf4d_n : $@convention(thin) () -> @owned E {
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

func ConcreteToArchetypeCastUInt8<T>(t: NotUInt8, t2: T) -> T {
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

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8xAA03NotI0V1t_x2t2tlFAD_Tg5Tf4nd_n : $@convention(thin) (NotUInt8) -> NotUInt8
// CHECK: bb0
// CHECK: return %0

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8xAA03NotI0V1t_x2t2tlFAA1CC_Tg5Tf4dd_n : $@convention(thin) () -> @owned C
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8xAA03NotI0V1t_x2t2tlFAA0J6UInt64V_Tg5Tf4dd_n : $@convention(thin) () -> NotUInt64
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ConcreteToArchetypeCastCxAA1CC1t_x2t2tlFAD_Tg5Tf4nd_n : $@convention(thin) (@owned C) -> @owned C
// CHECK: bb0
// CHECK: return %0

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ConcreteToArchetypeCastCxAA1CC1t_x2t2tlFAA8NotUInt8V_Tg5Tf4dd_n : $@convention(thin) () -> NotUInt8
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ConcreteToArchetypeCastCxAA1CC1t_x2t2tlFAA1DC_Tg5Tf4nd_n : $@convention(thin) (@owned C) -> @owned D
// CHECK: bb0
// CHECK:  checked_cast_br %0 : $C to $D
// CHECK: bb1

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ConcreteToArchetypeCastCxAA1CC1t_x2t2tlFAA1EC_Tg5Tf4dd_n : $@convention(thin) () -> @owned E
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch24ConcreteToArchetypeCastDxAA1DC1t_x2t2tlFAA1CC_Tg5Tf4nd_n : $@convention(thin) (@owned D) -> @owned C
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

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch21SuperToArchetypeCastCxAA1CC1c_x1ttlFAD_Tg5Tf4nd_n : $@convention(thin) (@owned C) -> @owned C
// CHECK: bb0
// CHECK: return %0 : $C

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch21SuperToArchetypeCastCxAA1CC1c_x1ttlFAA1DC_Tg5Tf4nd_n : $@convention(thin) (@owned C) -> @owned D
// CHECK: bb0
// CHECK:  checked_cast_br %0 : $C to $D
// CHECK: bb1

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch21SuperToArchetypeCastCxAA1CC1c_x1ttlFAA8NotUInt8V_Tg5Tf4dd_n : $@convention(thin) () -> NotUInt8
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch21SuperToArchetypeCastDxAA1DC1d_x1ttlFAA1CC_Tg5Tf4nd_n : $@convention(thin) (@owned D) -> @owned C
// CHECK: bb0
// CHECK:  [[T0:%.*]] = upcast %0 : $D to $C
// CHECK:  return [[T0]]

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch21SuperToArchetypeCastDxAA1DC1d_x1ttlFAD_Tg5Tf4nd_n : $@convention(thin) (@owned D) -> @owned D
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

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch26ExistentialToArchetypeCastxs9AnyObject_p1o_x1ttlFAA1CC_Tg5Tf4nd_n : $@convention(thin) (@owned AnyObject) -> @owned C
// CHECK: bb0
// CHECK:  checked_cast_br %0 : $AnyObject to $C
// CHECK: bb1

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch26ExistentialToArchetypeCastxs9AnyObject_p1o_x1ttlFAA8NotUInt8V_Tg5Tf4gd_n : $@convention(thin) (@guaranteed AnyObject) -> NotUInt8
// CHECK: bb0
// CHECK:  checked_cast_addr_br take_always AnyObject in {{%.*}} : $*AnyObject to NotUInt8 in {{%.*}} : $*NotUInt8,
// CHECK: bb1

// CHECK-LABEL: sil shared @_T030specialize_checked_cast_branch26ExistentialToArchetypeCastxs9AnyObject_p1o_x1ttlFsAC_p_Tg5Tf4nd_n : $@convention(thin) (@owned AnyObject) -> @owned AnyObject
// CHECK: bb0
// CHECK-NOT: checked_cast_br %
// CHECK: return %0 : $AnyObject
// CHECK-NOT: checked_cast_br %

_ = ExistentialToArchetypeCast(o: o, t: c)
_ = ExistentialToArchetypeCast(o: o, t: b)
_ = ExistentialToArchetypeCast(o: o, t: o)
