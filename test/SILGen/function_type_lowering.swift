// RUN: %target-swift-frontend -emit-silgen -target %target-swift-5.1-abi-triple -module-name main %s | %FileCheck %s


// Similarly-abstract generic signatures should share an unsubstituted type
// even in different generic contexts

// CHECK-LABEL: sil {{.*}}1a{{.*}} : $@convention(thin) <T, U> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U>) -> ()
func a<T, U>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1b{{.*}} : $@convention(thin) <T, U> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <U, T>) -> ()
func b<T, U>(_ x: (U) -> T) {}

// CHECK-LABEL: sil {{.*}}1c{{.*}} : $@convention(thin) <T, U, V> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <V, T>, @in_guaranteed U) -> ()
func c<T, U, V>(_ x: (V) -> T, _: U) {}

// CHECK-LABEL: sil {{.*}}003Hca{{.*}} : $@convention(thin) <T> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, T>) -> ()
func ç<T>(_ x: (T) -> T) {}

// CHECK-LABEL: sil {{.*}}returnsThrowing{{.*}} : $@convention(thin) <T, U, V> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0) -> (@owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> (@out τ_0_1, @error any Error) for <τ_0_1, τ_0_2>) for <T, U, V>) -> () {
func returnsThrowing<T, U, V>(_ x: (T) -> (U) throws -> V) {}


// ...including unconstrained associated types

protocol P {
  associatedtype A
}

// CHECK-LABEL: sil {{.*}}1d{{.*}} : $@convention(thin) <T where T : P> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, T.A>) -> ()
func d<T: P>(_ x: (T) -> T.A) {}

// CHECK-LABEL: sil {{.*}}1e{{.*}} : $@convention(thin) <T where T : P> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T.A, T>) -> ()
func e<T: P>(_ x: (T.A) -> T) {}


// Preserve class constraints, because they're less abstract for layout and
// calling convention purposes than unconstrained types

// CHECK-LABEL: sil {{.*}}1f{{.*}} : $@convention(thin) <T, U where T : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@guaranteed τ_0_0) -> @out τ_0_1 for <T, U>) -> ()
func f<T: AnyObject, U>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1g{{.*}} : $@convention(thin) <T, U where T : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_1 : AnyObject> (@in_guaranteed τ_0_0) -> @owned τ_0_1 for <U, T>) -> ()
func g<T: AnyObject, U>(_ x: (U) -> T) {}

// CHECK-LABEL: sil {{.*}}1h{{.*}} : $@convention(thin) <T, U where T : AnyObject, U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject, τ_0_1 : AnyObject> (@guaranteed τ_0_0) -> @owned τ_0_1 for <T, U>) -> ()
func h<T: AnyObject, U: AnyObject>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1i{{.*}} : $@convention(thin) <T, U where T : AnyObject, U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject, τ_0_1 : AnyObject> (@guaranteed τ_0_0) -> @owned τ_0_1 for <U, T>) -> ()
func i<T: AnyObject, U: AnyObject>(_ x: (U) -> T) {}


// Indirect class constraints

protocol PC: AnyObject { }

// CHECK-LABEL: sil {{.*}}1j{{.*}} : $@convention(thin) <T, U where T : PC> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@guaranteed τ_0_0) -> @out τ_0_1 for <T, U>) -> ()
func j<T: PC, U>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1k{{.*}} : $@convention(thin) <T, U where T : PC> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_1 : AnyObject> (@in_guaranteed τ_0_0) -> @owned τ_0_1 for <U, T>) -> ()
func k<T: PC, U>(_ x: (U) -> T) {}

// CHECK-LABEL: sil {{.*}}1l{{.*}} : $@convention(thin) <T, U where T : PC, U : PC> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject, τ_0_1 : AnyObject> (@guaranteed τ_0_0) -> @owned τ_0_1 for <T, U>) -> ()
func l<T: PC, U: PC>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1m{{.*}} : $@convention(thin) <T, U where T : PC, U : PC> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject, τ_0_1 : AnyObject> (@guaranteed τ_0_0) -> @owned τ_0_1 for <U, T>) -> ()
func m<T: PC, U: PC>(_ x: (U) -> T) {}

// CHECK-LABEL: sil {{.*}}1n{{.*}} : $@convention(thin) <T where T : P, T : PC> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@guaranteed τ_0_0) -> @out τ_0_1 for <T, T.A>) -> ()
func n<T: P & PC>(_ x: (T) -> T.A) {}


// Superclass constraints

class Base {}

// CHECK-LABEL: sil {{.*}}1o{{.*}} : $@convention(thin) <T, U where T : Base> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : _NativeClass> (@guaranteed τ_0_0) -> @out τ_0_1 for <T, U>) -> ()
func o<T: Base, U> (_ x: (T) -> U) {}


// Indirect constraints by associated type or protocol

protocol PCAO: AnyObject {
  associatedtype A
}

protocol POAC {
  associatedtype A: AnyObject
}

protocol PCAC: AnyObject {
  associatedtype A: AnyObject
}

// CHECK-LABEL: sil {{.*}}1p{{.*}} : $@convention(thin) <T where T : PCAO> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@guaranteed τ_0_0) -> @out τ_0_1 for <T, T.A>) -> ()
func p<T: PCAO> (_ x: (T) -> T.A) {}
// CHECK-LABEL: sil {{.*}}1q{{.*}} : $@convention(thin) <T where T : POAC> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_1 : AnyObject> (@in_guaranteed τ_0_0) -> @owned τ_0_1 for <T, T.A>) -> ()
func q<T: POAC> (_ x: (T) -> T.A) {}
// CHECK-LABEL: sil {{.*}}1r{{.*}} : $@convention(thin) <T where T : PCAC> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject, τ_0_1 : AnyObject> (@guaranteed τ_0_0) -> @owned τ_0_1 for <T, T.A>) -> ()
func r<T: PCAC> (_ x: (T) -> T.A) {}


// Structural positions

struct S<T, U> {
  struct Nested<V> { 
    struct NesNestedted<W> { }
    struct NestedNonGeneric { }
  }
  struct NestedNonGeneric {
    struct NesNestedted<W> { }
    struct NestedNonGeneric { }
  }
}

// CHECK-LABEL: sil {{.*}}1t{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_1 : AnyObject, τ_0_3 : AnyObject> (S<τ_0_0, τ_0_1>) -> (@out τ_0_2, @owned τ_0_3) for <T, U, T, U>) -> ()
func t<T, U: AnyObject>(_: (S<T, U>) -> (T, U)) {}

// CHECK-LABEL: sil {{.*}}2t2{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3, τ_0_4 where τ_0_1 : AnyObject, τ_0_2 : AnyObject, τ_0_4 : AnyObject> (S<τ_0_0, τ_0_1>.Nested<τ_0_2>) -> (@out τ_0_3, @owned τ_0_4) for <T, U, U, T, U>) -> ()
func t2<T, U: AnyObject>(_: (S<T, U>.Nested<U>) -> (T, U)) {}
// CHECK-LABEL: sil {{.*}}2t3{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3, τ_0_4, τ_0_5 where τ_0_1 : AnyObject, τ_0_2 : AnyObject, τ_0_5 : AnyObject> (S<τ_0_0, τ_0_1>.Nested<τ_0_2>.NesNestedted<τ_0_3>) -> (@out τ_0_4, @owned τ_0_5) for <T, U, U, T, T, U>) -> ()
func t3<T, U: AnyObject>(_: (S<T, U>.Nested<U>.NesNestedted<T>) -> (T, U)) {}
// CHECK-LABEL: sil {{.*}}2t4{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3, τ_0_4 where τ_0_1 : AnyObject, τ_0_2 : AnyObject, τ_0_4 : AnyObject> (S<τ_0_0, τ_0_1>.Nested<τ_0_2>.NestedNonGeneric) -> (@out τ_0_3, @owned τ_0_4) for <T, U, U, T, U>) -> ()
func t4<T, U: AnyObject>(_: (S<T, U>.Nested<U>.NestedNonGeneric) -> (T, U)) {}
// CHECK-LABEL: sil {{.*}}2t5{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_1 : AnyObject, τ_0_3 : AnyObject> (S<τ_0_0, τ_0_1>.NestedNonGeneric) -> (@out τ_0_2, @owned τ_0_3) for <T, U, T, U>) -> ()
func t5<T, U: AnyObject>(_: (S<T, U>.NestedNonGeneric) -> (T, U)) {}
// CHECK-LABEL: sil {{.*}}2t6{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3, τ_0_4 where τ_0_1 : AnyObject, τ_0_4 : AnyObject> (S<τ_0_0, τ_0_1>.NestedNonGeneric.NesNestedted<τ_0_2>) -> (@out τ_0_3, @owned τ_0_4) for <T, U, T, T, U>) -> ()
func t6<T, U: AnyObject>(_: (S<T, U>.NestedNonGeneric.NesNestedted<T>) -> (T, U)) {}
// CHECK-LABEL: sil {{.*}}2t7{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_1 : AnyObject, τ_0_3 : AnyObject> (S<τ_0_0, τ_0_1>.NestedNonGeneric.NestedNonGeneric) -> (@out τ_0_2, @owned τ_0_3) for <T, U, T, U>) -> ()
func t7<T, U: AnyObject>(_: (S<T, U>.NestedNonGeneric.NestedNonGeneric) -> (T, U)) {}

// CHECK-LABEL: sil {{.*}}1u{{.*}} : $@convention(thin) <T> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (S<τ_0_0, τ_0_1>) -> @out τ_0_2 for <T, T, T>) -> ()
func u<T>(_: (S<T, T>) -> T) {}


class C<T, U> {}

// CHECK-LABEL: sil {{.*}}1v{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_1 : AnyObject, τ_0_3 : AnyObject> (@guaranteed C<τ_0_0, τ_0_1>) -> (@out τ_0_2, @owned τ_0_3) for <T, U, T, U>) -> ()
func v<T, U: AnyObject>(_: (C<T, U>) -> (T, U)) {}

// CHECK-LABEL: sil {{.*}}1w{{.*}} : $@convention(thin) <T> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (@guaranteed C<τ_0_0, τ_0_1>) -> @out τ_0_2 for <T, T, T>) -> ()
func w<T>(_: (C<T, T>) -> T) {}

// CHECK-LABEL: sil {{.*}}1x{{.*}} : $@convention(thin) <T, U, V where V : C<T, U>> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0 where τ_0_0 : _NativeClass> (@guaranteed τ_0_0) -> () for <V>) -> ()
func x<T, U, V: C<T, U>>(_: (V) -> Void) {}

// We can't generally lower away protocol constraints 
// in nominal type argument positions, because they're necessary for the
// substitutions to be valid, and associated types may influence the ABI of
// the nominal type.

struct SP<T: P> { var x: T.A }
class CP<T: P> { }

// CHECK-LABEL: sil {{.*}}1z{{.*}} : $@convention(thin) <T where T : P> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0 where τ_0_0 : P> (@in_guaranteed SP<τ_0_0>) -> () for <T>) -> ()
func z<T: P>(_: (SP<T>) -> Void) {}

struct SCP<T: P, U: CP<T>> {}

// CHECK-LABEL: sil {{.*}}2z2{{.*}} : $@convention(thin) <T, U where T : P, U : CP<T>> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : P, τ_0_1 : CP<τ_0_0>> (SCP<τ_0_0, τ_0_1>) -> () for <T, U>) -> ()
func z2<T: P, U: CP<T>>(_: (SCP<T, U>) -> Void) {}

// CHECK-LABEL: sil {{.*}}3z2a{{.*}} : $@convention(thin) <T, U where T : AnyObject, T : P, U : CP<T>> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject, τ_0_0 : P, τ_0_1 : CP<τ_0_0>> (SCP<τ_0_0, τ_0_1>) -> () for <T, U>) -> ()
func z2a<T: P & AnyObject, U: CP<T>>(_: (SCP<T, U>) -> Void) {}

// CHECK-LABEL: sil {{.*}}2z3{{.*}} : $@convention(thin) <T, U where T : P, U : CP<T>> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : _NativeClass, τ_0_1 : _NativeClass> (S<τ_0_0, τ_0_1>) -> () for <U, U>) -> ()
func z3<T: P, U: CP<T>>(_: (S<U, U>) -> Void) {}

// Opaque types should not be extracted as substituted arguments because they
// aren't freely substitutable.

dynamic func opaqueAny() -> some Any { return C<Int, String>() }
dynamic func opaqueObject() -> some AnyObject { return C<Int, String>() }

// CHECK-LABEL: sil {{.*}}1y{{.*}} : $@convention(thin) @substituted <τ_0_0, τ_0_1 where τ_0_1 : AnyObject> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_1 : AnyObject> (@in_guaranteed τ_0_0) -> @owned τ_0_1 for <τ_0_0, τ_0_1>) -> () for <@_opaqueReturnTypeOf("$s4main9opaqueAnyQryF", 0) __, @_opaqueReturnTypeOf("$s4main12opaqueObjectQryF", 0) __>
func y(_: (@_opaqueReturnTypeOf("$s4main9opaqueAnyQryF", 0) X) -> (@_opaqueReturnTypeOf("$s4main12opaqueObjectQryF", 0) X)) {}

// Make sure type lowering doesn't choke on override signatures.
class Foo {
  func foo() -> Foo { return self }
}

class Bar<T>: Foo {
  override func foo() -> Bar<T> { return self }
}
