// RUN: %target-swift-frontend -emit-silgen -disable-availability-checking -module-name main -enable-subst-sil-function-types-for-function-values %s | %FileCheck %s


// Similarly-abstract generic signatures should share an unsubstituted type
// even in different generic contexts

// CHECK-LABEL: sil {{.*}}1a{{.*}} : $@convention(thin) <T, U> (@noescape @callee_guaranteed <τ_0_0, τ_0_1> in (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U>) -> ()
func a<T, U>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1b{{.*}} : $@convention(thin) <T, U> (@noescape @callee_guaranteed <τ_0_0, τ_0_1> in (@in_guaranteed τ_0_0) -> @out τ_0_1 for <U, T>) -> ()
func b<T, U>(_ x: (U) -> T) {}

// CHECK-LABEL: sil {{.*}}1c{{.*}} : $@convention(thin) <T, U, V> (@noescape @callee_guaranteed <τ_0_0, τ_0_1> in (@in_guaranteed τ_0_0) -> @out τ_0_1 for <V, T>, @in_guaranteed U) -> ()
func c<T, U, V>(_ x: (V) -> T, _: U) {}

// CHECK-LABEL: sil {{.*}}003Hca{{.*}} : $@convention(thin) <T> (@noescape @callee_guaranteed <τ_0_0, τ_0_1> in (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, T>) -> ()
func ç<T>(_ x: (T) -> T) {}


// ...including unconstrained associated types

protocol P {
  associatedtype A
}

// CHECK-LABEL: sil {{.*}}1d{{.*}} : $@convention(thin) <T where T : P> (@noescape @callee_guaranteed <τ_0_0, τ_0_1> in (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, T.A>) -> ()
func d<T: P>(_ x: (T) -> T.A) {}

// CHECK-LABEL: sil {{.*}}1e{{.*}} : $@convention(thin) <T where T : P> (@noescape @callee_guaranteed <τ_0_0, τ_0_1> in (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T.A, T>) -> ()
func e<T: P>(_ x: (T.A) -> T) {}


// Preserve class constraints, because they're less abstract for layout and
// calling convention purposes than unconstrained types

// CHECK-LABEL: sil {{.*}}1f{{.*}} : $@convention(thin) <T, U where T : AnyObject> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject> in (@guaranteed τ_0_0) -> @out τ_0_1 for <T, U>) -> ()
func f<T: AnyObject, U>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1g{{.*}} : $@convention(thin) <T, U where T : AnyObject> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_1 : _RefCountedObject> in (@in_guaranteed τ_0_0) -> @owned τ_0_1 for <U, T>) -> ()
func g<T: AnyObject, U>(_ x: (U) -> T) {}

// CHECK-LABEL: sil {{.*}}1h{{.*}} : $@convention(thin) <T, U where T : AnyObject, U : AnyObject> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject, τ_0_1 : _RefCountedObject> in (@guaranteed τ_0_0) -> @owned τ_0_1 for <T, U>) -> ()
func h<T: AnyObject, U: AnyObject>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1i{{.*}} : $@convention(thin) <T, U where T : AnyObject, U : AnyObject> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject, τ_0_1 : _RefCountedObject> in (@guaranteed τ_0_0) -> @owned τ_0_1 for <U, T>) -> ()
func i<T: AnyObject, U: AnyObject>(_ x: (U) -> T) {}


// Indirect class constraints

protocol PC: AnyObject { }

// CHECK-LABEL: sil {{.*}}1j{{.*}} : $@convention(thin) <T, U where T : PC> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject> in (@guaranteed τ_0_0) -> @out τ_0_1 for <T, U>) -> ()
func j<T: PC, U>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1k{{.*}} : $@convention(thin) <T, U where T : PC> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_1 : _RefCountedObject> in (@in_guaranteed τ_0_0) -> @owned τ_0_1 for <U, T>) -> ()
func k<T: PC, U>(_ x: (U) -> T) {}

// CHECK-LABEL: sil {{.*}}1l{{.*}} : $@convention(thin) <T, U where T : PC, U : PC> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject, τ_0_1 : _RefCountedObject> in (@guaranteed τ_0_0) -> @owned τ_0_1 for <T, U>) -> ()
func l<T: PC, U: PC>(_ x: (T) -> U) {}

// CHECK-LABEL: sil {{.*}}1m{{.*}} : $@convention(thin) <T, U where T : PC, U : PC> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject, τ_0_1 : _RefCountedObject> in (@guaranteed τ_0_0) -> @owned τ_0_1 for <U, T>) -> ()
func m<T: PC, U: PC>(_ x: (U) -> T) {}

// CHECK-LABEL: sil {{.*}}1n{{.*}} : $@convention(thin) <T where T : P, T : PC> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject> in (@guaranteed τ_0_0) -> @out τ_0_1 for <T, T.A>) -> ()
func n<T: P & PC>(_ x: (T) -> T.A) {}


// Superclass constraints

class Base {}

// CHECK-LABEL: sil {{.*}}1o{{.*}} : $@convention(thin) <T, U where T : Base> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject> in (@guaranteed τ_0_0) -> @out τ_0_1 for <T, U>) -> ()
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

// CHECK-LABEL: sil {{.*}}1p{{.*}} : $@convention(thin) <T where T : PCAO> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject> in (@guaranteed τ_0_0) -> @out τ_0_1 for <T, T.A>) -> ()
func p<T: PCAO> (_ x: (T) -> T.A) {}
// CHECK-LABEL: sil {{.*}}1q{{.*}} : $@convention(thin) <T where T : POAC> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_1 : _RefCountedObject> in (@in_guaranteed τ_0_0) -> @owned τ_0_1 for <T, T.A>) -> ()
func q<T: POAC> (_ x: (T) -> T.A) {}
// CHECK-LABEL: sil {{.*}}1r{{.*}} : $@convention(thin) <T where T : PCAC> (@noescape @callee_guaranteed <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject, τ_0_1 : _RefCountedObject> in (@guaranteed τ_0_0) -> @owned τ_0_1 for <T, T.A>) -> ()
func r<T: PCAC> (_ x: (T) -> T.A) {}


// Structural positions

struct S<T, U> {}

// CHECK-LABEL: sil {{.*}}1t{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@noescape @callee_guaranteed <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_1 : _RefCountedObject, τ_0_3 : _RefCountedObject> in (S<τ_0_0, τ_0_1>) -> (@out τ_0_2, @owned τ_0_3) for <T, U, T, U>) -> ()
func t<T, U: AnyObject>(_: (S<T, U>) -> (T, U)) {}

// CHECK-LABEL: sil {{.*}}1u{{.*}} : $@convention(thin) <T> (@noescape @callee_guaranteed <τ_0_0, τ_0_1, τ_0_2> in (S<τ_0_0, τ_0_1>) -> @out τ_0_2 for <T, T, T>) -> ()
func u<T>(_: (S<T, T>) -> T) {}


class C<T, U> {}

// CHECK-LABEL: sil {{.*}}1v{{.*}} : $@convention(thin) <T, U where U : AnyObject> (@noescape @callee_guaranteed <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_1 : _RefCountedObject, τ_0_3 : _RefCountedObject> in (@guaranteed C<τ_0_0, τ_0_1>) -> (@out τ_0_2, @owned τ_0_3) for <T, U, T, U>) -> ()
func v<T, U: AnyObject>(_: (C<T, U>) -> (T, U)) {}

// CHECK-LABEL: sil {{.*}}1w{{.*}} : $@convention(thin) <T> (@noescape @callee_guaranteed <τ_0_0, τ_0_1, τ_0_2> in (@guaranteed C<τ_0_0, τ_0_1>) -> @out τ_0_2 for <T, T, T>) -> ()
func w<T>(_: (C<T, T>) -> T) {}

// CHECK-LABEL: sil {{.*}}1x{{.*}} : $@convention(thin) <T, U, V where V : C<T, U>> (@noescape @callee_guaranteed <τ_0_0 where τ_0_0 : _RefCountedObject> in (@guaranteed τ_0_0) -> () for <V>) -> ()
func x<T, U, V: C<T, U>>(_: (V) -> Void) {}


// Opaque types should not be extracted as substituted arguments because they
// aren't freely substitutable.

dynamic func opaqueAny() -> some Any { return C<Int, String>() }
dynamic func opaqueObject() -> some AnyObject { return C<Int, String>() }

// CHECK-LABEL: sil {{.*}}1y{{.*}} : $@convention(thin) (@noescape @callee_guaranteed (@in_guaranteed @_opaqueReturnTypeOf("$s4main9opaqueAnyQryF", 0) {{.*}}) -> @owned @_opaqueReturnTypeOf("$s4main12opaqueObjectQryF", 0) {{.*}}) -> ()
func y(_: (@_opaqueReturnTypeOf("$s4main9opaqueAnyQryF", 0) X) -> (@_opaqueReturnTypeOf("$s4main12opaqueObjectQryF", 0) X)) {}
