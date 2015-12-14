// RUN: %target-swift-frontend -emit-sil -o - -O %s | FileCheck %s

//////////////////
// Declarations //
//////////////////

public class C {}
public class D : C {}
public class E {}

var b : UInt8 = 0
var c = C()
var d = D()
var e = E()
var f : UInt64 = 0
var o : AnyObject = c

////////////////////////////
// Archetype To Archetype //
////////////////////////////

@inline(never)
public func ArchetypeToArchetype<T1, T2>(t t: T1, t2: T2) -> T2 {
  return t as! T2
}

ArchetypeToArchetype(t: b, t2: b)
ArchetypeToArchetype(t: c, t2: c)
ArchetypeToArchetype(t: b, t2: c)
ArchetypeToArchetype(t: c, t2: b)
ArchetypeToArchetype(t: c, t2: d)
ArchetypeToArchetype(t: d, t2: c)
ArchetypeToArchetype(t: c, t2: e)
ArchetypeToArchetype(t: b, t2: f)

// x -> y where x and y are unrelated non classes.
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs5UInt8_Vs6UInt64___TF37specialize_unconditional_checked_cast20ArchetypeToArchetype{{.*}} : $@convention(thin) (@out UInt64, @in UInt8, @in UInt64) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK:      builtin "int_trap"
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype

// x -> y where x and y are unrelated classes.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C_CS_1E___TF37specialize_unconditional_checked_cast20ArchetypeToArchetype{{.*}} : $@convention(thin) (@out E, @in C, @in E) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: builtin "int_trap"
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype

// y -> x where x is a super class of y.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1D_CS_1C___TF37specialize_unconditional_checked_cast20ArchetypeToArchetype{{.*}} : $@convention(thin) (@out C, @in D, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: upcast {{%[0-9]+}} : $D to $C
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C_CS_1D___TF37specialize_unconditional_checked_cast20ArchetypeToArchetype{{.*}} : $@convention(thin) (@out D, @in C, @in D) -> () {
// CHECK: unconditional_checked_cast_addr take_always C in %1 : $*C to D in

// y -> x where x is not a class but y is.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C_Vs5UInt8___TF37specialize_unconditional_checked_cast20ArchetypeToArchetype{{.*}} : $@convention(thin) (@out UInt8, @in C, @in UInt8) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype
// CHECK: builtin "int_trap"
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype

// x -> y where x is not a class but y is.
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs5UInt8_C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast20ArchetypeToArchetype{{.*}} : $@convention(thin) (@out C, @in UInt8, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast_addr
// CHECK-NOT: unconditional_checked_cast_addr
// CHECK:     builtin "int_trap"
// CHECK-NOT: unconditional_checked_cast_addr

// x -> x where x is a class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C_S0____TF37specialize_unconditional_checked_cast20ArchetypeToArchetype{{.*}} : $@convention(thin) (@out C, @in C, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype

// x -> x where x is not a class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs5UInt8_S____TF37specialize_unconditional_checked_cast20ArchetypeToArchetype{{.*}} : $@convention(thin) (@out UInt8, @in UInt8, @in UInt8) -> () {
// CHECK-NOT: unconditional_checked_cast archetype_to_archetype


///////////////////////////
// Archetype To Concrete //
///////////////////////////

@inline(never)
public func ArchetypeToConcreteConvertUInt8<T>(t t: T) -> UInt8 {
  return t as! UInt8
}
ArchetypeToConcreteConvertUInt8(t: b)
ArchetypeToConcreteConvertUInt8(t: c)
ArchetypeToConcreteConvertUInt8(t: f)

// order 57%
// x -> x where x is not a class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs5UInt8___TF37specialize_unconditional_checked_cast31ArchetypeToConcreteConvertUInt8{{.*}} : $@convention(thin) (@in UInt8) -> UInt8 {
// CHECK: bb0
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: load
// CHECK-NEXT: return

// order: 59%
// x -> y where x,y are classes and x is a super class of y.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1D___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertC{{.*}} : $@convention(thin) (@in D) -> @owned C {
// CHECK: bb0
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: load
// CHECK-NEXT: upcast
// CHECK-NEXT: return

// order: 60%
// x -> x where x is a class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertC{{.*}} : $@convention(thin) (@in C) -> @owned C {
// CHECK: bb0
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: load
// CHECK-NEXT: return


@inline(never)
public func ArchetypeToConcreteConvertC<T>(t t: T) -> C {
  return t as! C
}

ArchetypeToConcreteConvertC(t: c)
ArchetypeToConcreteConvertC(t: b)
ArchetypeToConcreteConvertC(t: d)
ArchetypeToConcreteConvertC(t: e)




@inline(never)
public func ArchetypeToConcreteConvertD<T>(t t: T) -> D {
  return t as! D
}

ArchetypeToConcreteConvertD(t: c)

// x -> y where x,y are classes and x is a sub class of y.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertD{{.*}} : $@convention(thin) (@in C) -> @owned D {
// CHECK: bb0
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: alloc_stack
// CHECK-NEXT: unconditional_checked_cast_addr take_always
// CHECK-NEXT: load
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: return

@inline(never)
public func ArchetypeToConcreteConvertE<T>(t t: T) -> E {
  return t as! E
}

ArchetypeToConcreteConvertE(t: c)


///////////////////////////
// Concrete to Archetype //
///////////////////////////

@inline(never)
public func ConcreteToArchetypeConvertUInt8<T>(t t: UInt8, t2: T) -> T {
  return t as! T
}

ConcreteToArchetypeConvertUInt8(t: b, t2: b)
ConcreteToArchetypeConvertUInt8(t: b, t2: c)
ConcreteToArchetypeConvertUInt8(t: b, t2: f)

// x -> y where x,y are different non class types.
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs6UInt64___TF37specialize_unconditional_checked_cast31ConcreteToArchetypeConvertUInt8{{.*}} : $@convention(thin) (@out UInt64, UInt8, @in UInt64) -> () {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

// x -> y where x is not a class but y is a class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast31ConcreteToArchetypeConvertUInt8{{.*}} : $@convention(thin) (@out C, UInt8, @in C) -> () {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

// x -> x where x is not a class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs5UInt8___TF37specialize_unconditional_checked_cast31ConcreteToArchetypeConvertUInt8{{.*}} : $@convention(thin) (@out UInt8, UInt8, @in UInt8) -> () {
// CHECK: bb0
// CHECK-NEXT: debug_value
// CHECK-NEXT: store
// CHECK-NEXT: tuple
// CHECK-NEXT: return

@inline(never)
public func ConcreteToArchetypeConvertC<T>(t t: C, t2: T) -> T {
  return t as! T
}

ConcreteToArchetypeConvertC(t: c, t2: c)
ConcreteToArchetypeConvertC(t: c, t2: b)
ConcreteToArchetypeConvertC(t: c, t2: d)
ConcreteToArchetypeConvertC(t: c, t2: e)

// x -> y where x and y are unrelated classes.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1E___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertC{{.*}} : $@convention(thin) (@out E, @owned C, @in E) -> () {
// CHECK: bb0
// CHECK-NEXT: debug_value
// CHECK-NEXT: strong_retain
// CHECK-NEXT: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1D___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertC{{.*}} : $@convention(thin) (@out D, @owned C, @in D) -> () {
// CHECK: bb0
// CHECK-NEXT: debug_value
// CHECK-NEXT: debug_value_addr
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

// x -> y where x is a class but y is not.
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs5UInt8___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertC{{.*}} : $@convention(thin) (@out UInt8, @owned C, @in UInt8) -> () {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

// x -> x where x is a class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertC{{.*}} : $@convention(thin) (@out C, @owned C, @in C) -> () {
// CHECK: bb0
// CHECK-NEXT: debug_value
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return

@inline(never)
public func ConcreteToArchetypeConvertD<T>(t t: D, t2: T) -> T {
  return t as! T
}

ConcreteToArchetypeConvertD(t: d, t2: c)

// x -> y where x is a subclass of y.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ConcreteToArchetypeConvertD{{.*}} : $@convention(thin) (@out C, @owned D, @in C) -> () {
// CHECK: bb0
// CHECK-NEXT: debug_value
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: upcast
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return


////////////////////////
// Super To Archetype //
////////////////////////

@inline(never)
public func SuperToArchetypeC<T>(c c : C, t : T) -> T {
  return c as! T
}

SuperToArchetypeC(c: c, t: c)
SuperToArchetypeC(c: c, t: d)
SuperToArchetypeC(c: c, t: b)

// x -> y where x is a class and y is not.
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs5UInt8___TF37specialize_unconditional_checked_cast17SuperToArchetypeC{{.*}} : $@convention(thin) (@out UInt8, @owned C, @in UInt8) -> () {
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

// x -> y where x is a super class of y.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1D___TF37specialize_unconditional_checked_cast17SuperToArchetypeC{{.*}} : $@convention(thin) (@out D, @owned C, @in D) -> () {
// CHECK: bb0
// CHECK: unconditional_checked_cast_addr take_always C in

// x -> x where x is a class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast17SuperToArchetypeC{{.*}} : $@convention(thin) (@out C, @owned C, @in C) -> () {
// CHECK: bb0
// CHECK-NEXT: debug_value
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return

@inline(never)
public func SuperToArchetypeD<T>(d d : D, t : T) -> T {
  return d as! T
}

SuperToArchetypeD(d: d, t: c)
SuperToArchetypeD(d: d, t: d)

// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1D___TF37specialize_unconditional_checked_cast17SuperToArchetypeD{{.*}} : $@convention(thin) (@out D, @owned D, @in D) -> () {
// CHECK: bb0
// CHECK-NEXT: debug_value
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return

// *NOTE* The frontend is smart enough to turn this into an upcast. When this
// test is converted to SIL, this should be fixed appropriately.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast17SuperToArchetypeD{{.*}} : $@convention(thin) (@out C, @owned D, @in C) -> () {
// CHECK-NOT: unconditional_checked_cast super_to_archetype
// CHECK: upcast
// CHECK-NOT: unconditional_checked_cast super_to_archetype

//////////////////////////////
// Existential To Archetype //
//////////////////////////////

@inline(never)
public func ExistentialToArchetype<T>(o o : AnyObject, t : T) -> T {
  return o as! T
}

// AnyObject -> AnyObject
// CHECK-LABEL: sil shared [noinline] @_TTSg5Ps9AnyObject____TF37specialize_unconditional_checked_cast22ExistentialToArchetype{{.*}} : $@convention(thin) (@out AnyObject, @owned AnyObject, @in AnyObject) -> () {
// CHECK: bb0
// CHECK-NEXT: debug_value
// CHECK-NEXT: debug_value_addr
// CHECK-NEXT: store
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return

// AnyObject -> Non Class (should always fail)
// CHECK-LABEL: sil shared [noinline] @_TTSg5Vs5UInt8___TF37specialize_unconditional_checked_cast22ExistentialToArchetype{{.*}} : $@convention(thin) (@out UInt8, @owned AnyObject, @in UInt8) -> () {
// CHECK: builtin "int_trap"()
// CHECK: unreachable
// CHECK-NEXT: }

// AnyObject -> Class.
// CHECK-LABEL: sil shared [noinline] @_TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast22ExistentialToArchetype{{.*}} : $@convention(thin) (@out C, @owned AnyObject, @in C) -> () {
// CHECK: unconditional_checked_cast_addr take_always AnyObject in {{%.*}} : $*AnyObject to C

ExistentialToArchetype(o: o, t: c)
ExistentialToArchetype(o: o, t: b)
ExistentialToArchetype(o: o, t: o)

// Ensure that a downcast from an Optional source is not promoted to a
// value cast. We could do the promotion, but the optimizer would need
// to insert the Optional unwrapping logic before the cast.
//
// CHECK-LABEL: sil shared [noinline] @_TTSg5GSqC37specialize_unconditional_checked_cast1C__CS_1D___TF37specialize_unconditional_checked_cast15genericDownCastu0_rFTxMq__q_ : $@convention(thin) (@out D, @in Optional<C>, @thick D.Type) -> () {
// CHECK: unconditional_checked_cast_addr take_always Optional<C> in %1 : $*Optional<C> to D in %0 : $*D
@inline(never)
public func genericDownCast<T, U>(a: T, _ : U.Type) -> U {
  return a as! U
}

public func callGenericDownCast(c: C?) -> D {
  return genericDownCast(c, D.self)
}

//order: -5
// x -> y where y is a class but x is not.
// CHECK-LABEL: sil shared [noinline] @_TTSf4d___TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast31ArchetypeToConcreteConvertUInt8
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

//order: -4
// x -> y where x,y are not classes and x is a different type from y.
// CHECK-LABEL: sil shared [noinline] @_TTSf4d___TTSg5Vs6UInt64___TF37specialize_unconditional_checked_cast31ArchetypeToConcreteConvertUInt8
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

// order -3
// x -> y where x is a class but y is not.
// CHECK-LABEL: sil shared [noinline] @_TTSf4d___TTSg5Vs5UInt8___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertC
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

// order -2
// x -> y where x,y are classes, but x is unrelated to y.
// CHECK-LABEL: sil shared [noinline] @_TTSf4d___TTSg5C37specialize_unconditional_checked_cast1E___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertC
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }

// order -1
// x -> y where x,y are classes, but y is unrelated to x. The idea is
// to make sure that the fact that y is concrete does not affect the
// result.
// CHECK-LABEL: sil shared [noinline] @_TTSf4d___TTSg5C37specialize_unconditional_checked_cast1C___TF37specialize_unconditional_checked_cast27ArchetypeToConcreteConvertE
// CHECK: bb0
// CHECK: builtin "int_trap"
// CHECK: unreachable
// CHECK-NEXT: }
