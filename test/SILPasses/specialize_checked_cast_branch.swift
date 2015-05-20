// RUN: %target-swift-frontend -disable-func-sig-opts -emit-sil -O -sil-inline-threshold 0 %s -o - | not FileCheck %s

// FIXME: rdar://problem/18603827

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

func ArchetypeToArchetypeCast<T1, T2>(t1 t1 : T1, t2 : T2) -> T2 {
  if let x = t1 as? T2 {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// x -> x where x is a class.
// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C_S0____TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@convention(thin) (@out C, @in C, @in C) -> () {
// CHECK: [[T0:%.*]] = load %1 : $*C
// CHECK: enum $Optional<C>, #Optional.Some!enumelt.1, [[T0]] : $C
// CHECK: strong_retain [[T0]]
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: c, t2: c)

// x -> x where x is not a class.
// CHECK-LABEL: sil shared @_TTSgVSs5UInt8_S____TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@convention(thin) (@out UInt8, @in UInt8, @in UInt8) -> () {
// CHECK: [[T0:%.*]] = load %1 : $*UInt8
// CHECK: enum $Optional<UInt8>, #Optional.Some!enumelt.1, [[T0]] : $UInt8
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: b, t2: b)

// x -> y where x,y are not classes and x is a different type from y.
// CHECK-LABEL: sil shared @_TTSgVSs5UInt8_VSs6UInt64___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@convention(thin) (@out UInt64, @in UInt8, @in UInt64) -> () {
// CHECK: enum $Optional<UInt64>, #Optional.None
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: b, t2: f)

// x -> y where x is not a class but y is.
// CHECK-LABEL: sil shared @_TTSgVSs5UInt8_C30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@convention(thin) (@out C, @in UInt8, @in C) -> () {
// CHECK: enum $Optional<C>, #Optional.None
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: b, t2: c)

// y -> x where x is a class but y is not.
// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C_VSs5UInt8___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@convention(thin) (@out UInt8, @in C, @in UInt8) -> () {
// CHECK: enum $Optional<UInt8>, #Optional.None
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: c, t2: b)

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C_CS_1D___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@convention(thin) (@out D, @in C, @in D) -> () {
// CHECK:   [[TMP:%.*]] = alloc_stack $Optional<D>
// CHECK:   [[V:%.*]] = load %1 : $*C
// CHECK:   checked_cast_br [[V]] : $C to $D,
// CHECK: bb1([[T0:%.*]] : $D):
// CHECK:   [[T1:%.*]] = enum $Optional<D>, #Optional.Some!enumelt.1, [[T0]] : $D
// CHECK:   store [[T1]] to [[TMP]]#1 : $*Optional<D>
// CHECK:   strong_retain [[V]] : $C
// CHECK:   br bb3
// CHECK: bb2:
// CHECK:   [[T0:%.*]] = enum $Optional<D>, #Optional.None
// CHECK:   store [[T0]] to [[TMP]]#1 : $*Optional<D>
// CHECK:   br bb3
ArchetypeToArchetypeCast(t1: c, t2: d)

// y -> x where x is a super class of y.
// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1D_CS_1C___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@convention(thin) (@out C, @in D, @in C) -> () {
// CHECK: [[T0:%.*]] = load %1 : $*D
// CHECK: [[T1:%.*]] = upcast [[T0]] : $D to $C
// CHECK: enum $Optional<C>, #Optional.Some!enumelt.1, [[T1]] : $C
// CHECK: strong_retain [[T0]] : $D
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: d, t2: c)

// x -> y where x and y are unrelated.
// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C_CS_1E___TF30specialize_checked_cast_branch24ArchetypeToArchetypeCastU___FT2t1Q_2t2Q0__Q0_ : $@convention(thin) (@out E, @in C, @in E) -> () {
// CHECK: enum $Optional<E>, #Optional.None
// CHECK: bb1:
ArchetypeToArchetypeCast(t1: c, t2: e)

///////////////////////////
// Archetype To Concrete //
///////////////////////////

func ArchetypeToConcreteCastUInt8<T>(t t : T) -> UInt8 {
  if let x = t as? UInt8 {
    return x
  }
  _preconditionFailure("??? Profit?")
}

func ArchetypeToConcreteCastC<T>(t t : T) -> C {
  if let x = t as? C {
    return x
  }
  _preconditionFailure("??? Profit?")
}

func ArchetypeToConcreteCastD<T>(t t : T) -> D {
  if let x = t as? D {
    return x
  }
  _preconditionFailure("??? Profit?")
}

func ArchetypeToConcreteCastE<T>(t t : T) -> E {
  if let x = t as? E {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSgVSs5UInt8___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8U__FT1tQ__VSs5UInt8 : $@convention(thin) (@in UInt8) -> UInt8 {
// CHECK-NEXT: bb0
// CHECK-NEXT: [[VALUE:%.*]] = load %0 : $*UInt8
ArchetypeToConcreteCastUInt8(t: b)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8U__FT1tQ__VSs5UInt8 : $@convention(thin) (@in C) -> UInt8 {
// CHECK: bb0
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable
ArchetypeToConcreteCastUInt8(t: c)

// CHECK-LABEL: sil shared @_TTSgVSs6UInt64___TF30specialize_checked_cast_branch28ArchetypeToConcreteCastUInt8U__FT1tQ__VSs5UInt8 : $@convention(thin) (@in UInt64) -> UInt8 {
// CHECK-NEXT: bb0
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable
ArchetypeToConcreteCastUInt8(t: f)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCU__FT1tQ__CS_1C : $@convention(thin) (@in C) -> @owned C {
// CHECK-NEXT: bb0
// CHECK-NEXT:  [[VALUE:%.*]] = load %0 : $*C
ArchetypeToConcreteCastC(t: c)

// CHECK-LABEL: sil shared @_TTSgVSs5UInt8___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCU__FT1tQ__CS_1C : $@convention(thin) (@in UInt8) -> @owned C {
// CHECK: bb0
// CHECK-NOT: checked_cast_br archetype_to_concrete
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable
ArchetypeToConcreteCastC(t: b)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCU__FT1tQ__CS_1C : $@convention(thin) (@in D) -> @owned C {
// CHECK: bb0
// CHECK:  [[VALUE:%.*]] = load %0 : $*D
// CHECK:  [[CAST:%.*]] = upcast [[VALUE]] : $D to $C
// CHECK: return [[CAST]]
ArchetypeToConcreteCastC(t: d)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1E___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastCU__FT1tQ__CS_1C : $@convention(thin) (@in E) -> @owned C {
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable
ArchetypeToConcreteCastC(t: e)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastDU__FT1tQ__CS_1D : $@convention(thin) (@in C) -> @owned D {
// CHECK: bb0
// CHECK:  [[T0:%.*]] = load %0 : $*C
// CHECK:  checked_cast_br [[T0]] : $C to $D, bb1, bb2
// CHECK: bb1(
// CHECK: strong_retain [[T0]] : $C
// CHECK: bb0
ArchetypeToConcreteCastD(t: c)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ArchetypeToConcreteCastEU__FT1tQ__CS_1E : $@convention(thin) (@in C) -> @owned E {
// CHECK: bb0
// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_fail [[TRUE]]
// CHECK: unreachable
ArchetypeToConcreteCastE(t: c)

///////////////////////////
// Concrete To Archetype //
///////////////////////////

func ConcreteToArchetypeCastUInt8<T>(t t: UInt8, t2: T) -> T {
  if let x = t as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}
func ConcreteToArchetypeCastC<T>(t t: C, t2: T) -> T {
  if let x = t as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}
func ConcreteToArchetypeCastD<T>(t t: D, t2: T) -> T {
  if let x = t as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSgVSs5UInt8___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@convention(thin) (@out UInt8, UInt8, @in UInt8) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<UInt8>, #Optional.Some!enumelt.1, %1
// CHECK: bb1
ConcreteToArchetypeCastUInt8(t: b, t2: b)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@convention(thin) (@out C, UInt8, @in C) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<C>, #Optional.None
// CHECK: bb1
ConcreteToArchetypeCastUInt8(t: b, t2: c)

// CHECK-LABEL: sil shared @_TTSgVSs6UInt64___TF30specialize_checked_cast_branch28ConcreteToArchetypeCastUInt8U__FT1tVSs5UInt82t2Q__Q_ : $@convention(thin) (@out UInt64, UInt8, @in UInt64) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<UInt64>, #Optional.None
// CHECK: bb1
ConcreteToArchetypeCastUInt8(t: b, t2: f)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCU__FT1tCS_1C2t2Q__Q_ : $@convention(thin) (@out C, @owned C, @in C) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<C>, #Optional.Some!enumelt.1, %1
// CHECK: bb1
ConcreteToArchetypeCastC(t: c, t2: c)

// CHECK-LABEL: sil shared @_TTSgVSs5UInt8___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCU__FT1tCS_1C2t2Q__Q_ : $@convention(thin) (@out UInt8, @owned C, @in UInt8) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<UInt8>, #Optional.None
// CHECK: bb1
ConcreteToArchetypeCastC(t: c, t2: b)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCU__FT1tCS_1C2t2Q__Q_ : $@convention(thin) (@out D, @owned C, @in D) -> () {
// CHECK: bb0
// CHECK:  checked_cast_br %1 : $C to $D
// CHECK: bb1
ConcreteToArchetypeCastC(t: c, t2: d)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1E___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastCU__FT1tCS_1C2t2Q__Q_ : $@convention(thin) (@out E, @owned C, @in E) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<E>, #Optional.None
// CHECK: bb1
ConcreteToArchetypeCastC(t: c, t2: e)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch24ConcreteToArchetypeCastDU__FT1tCS_1D2t2Q__Q_ : $@convention(thin) (@out C, @owned D, @in C) -> () {
// CHECK: bb0
// CHECK:  [[T0:%.*]] = upcast %1 : $D to $C
// CHECK:  enum $Optional<C>, #Optional.Some!enumelt.1, [[T0]] : $C
// CHECK: bb1
ConcreteToArchetypeCastD(t: d, t2: c)

////////////////////////
// Super To Archetype //
////////////////////////

func SuperToArchetypeCastC<T>(c c : C, t : T) -> T {
  if let x = c as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}

func SuperToArchetypeCastD<T>(d d : D, t : T) -> T {
  if let x = d as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch21SuperToArchetypeCastCU__FT1cCS_1C1tQ__Q_ : $@convention(thin) (@out C, @owned C, @in C) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<C>, #Optional.Some!enumelt.1, %1
// CHECK: bb1
SuperToArchetypeCastC(c: c, t: c)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch21SuperToArchetypeCastCU__FT1cCS_1C1tQ__Q_ : $@convention(thin) (@out D, @owned C, @in D) -> () {
// CHECK: bb0
// CHECK:  checked_cast_br %1 : $C to $D
// CHECK: bb1
SuperToArchetypeCastC(c: c, t: d)

// CHECK-LABEL: sil shared @_TTSgVSs5UInt8___TF30specialize_checked_cast_branch21SuperToArchetypeCastCU__FT1cCS_1C1tQ__Q_ : $@convention(thin) (@out UInt8, @owned C, @in UInt8) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<UInt8>, #Optional.None
// CHECK: bb1
SuperToArchetypeCastC(c: c, t: b)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch21SuperToArchetypeCastDU__FT1dCS_1D1tQ__Q_ : $@convention(thin) (@out C, @owned D, @in C) -> () {
// CHECK: bb0
// CHECK:  [[T0:%.*]] = upcast %1 : $D to $C
// CHECK:  enum $Optional<C>, #Optional.Some!enumelt.1, [[T0]] : $C
// CHECK: bb1
SuperToArchetypeCastD(d: d, t: c)

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1D___TF30specialize_checked_cast_branch21SuperToArchetypeCastDU__FT1dCS_1D1tQ__Q_ : $@convention(thin) (@out D, @owned D, @in D) -> () {
// CHECK: bb0
// CHECK:  enum $Optional<D>, #Optional.Some!enumelt.1, %1
// CHECK: bb1
SuperToArchetypeCastD(d: d, t: d)

//////////////////////////////
// Existential To Archetype //
//////////////////////////////

func ExistentialToArchetypeCast<T>(o o : AnyObject, t : T) -> T {
  if let x = o as? T {
    return x
  }
  _preconditionFailure("??? Profit?")
}

// CHECK-LABEL: sil shared @_TTSgC30specialize_checked_cast_branch1C___TF30specialize_checked_cast_branch26ExistentialToArchetypeCastU__FT1oPSs9AnyObject_1tQ__Q_ : $@convention(thin) (@out C, @owned AnyObject, @in C) -> () {
// CHECK: bb0
// CHECK:  checked_cast_br %1 : $AnyObject to $C
// CHECK: bb1
ExistentialToArchetypeCast(o: o, t: c)

// CHECK-LABEL: sil shared @_TTSgVSs5UInt8___TF30specialize_checked_cast_branch26ExistentialToArchetypeCastU__FT1oPSs9AnyObject_1tQ__Q_ : $@convention(thin) (@out UInt8, @owned AnyObject, @in UInt8) -> () {
// CHECK: bb0
// CHECK:  checked_cast_addr_br take_always AnyObject in {{%.*}} : $*AnyObject to UInt8 in {{%.*}} : $*UInt8,
// CHECK: bb1
ExistentialToArchetypeCast(o: o, t: b)

// CHECK-LABEL: sil shared @_TTSgPSs9AnyObject____TF30specialize_checked_cast_branch26ExistentialToArchetypeCastU__FT1oPSs9AnyObject_1tQ__Q_ : $@convention(thin) (@out AnyObject, @owned AnyObject, @in AnyObject) -> () {
// CHECK: bb0
// CHECK-NOT: checked_cast_br %
// CHECK:  enum $Optional<AnyObject>, #Optional.Some!enumelt.1, %1 : $AnyObject
// CHECK-NOT: checked_cast_br %
// CHECK: bb1
ExistentialToArchetypeCast(o: o, t: o)
