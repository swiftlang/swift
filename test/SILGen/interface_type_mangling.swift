// RUN: %target-swift-frontend %s -emit-silgen -enable-interface-type-mangling | FileCheck %s

protocol P {
  typealias Assoc1
  typealias Assoc2
}
protocol PP: P {}
protocol PQ: P {
  typealias Assoc1: A
}
protocol Q {
  typealias Assoc0: A
}

protocol A {
  typealias Assoc
}

class Base: Q, A {
  typealias Assoc = Base
  typealias Assoc0 = Base
}

// CHECK-LABEL: interface_type_mangling.f1 : <T_0_0 where T_0_0: interface_type_mangling.PP, T_0_0: interface_type_mangling.PQ> (T_0_0) -> ()
func f1<T where T: PP, T: PQ>(x: T) {}
// CHECK-LABEL: interface_type_mangling.f2 : <T_0_0 where T_0_0: interface_type_mangling.PP, T_0_0: interface_type_mangling.PQ> (T_0_0) -> ()
func f2<T where T: PQ, T: PP>(x: T) {}
// CHECK-LABEL: interface_type_mangling.f3 : <T_0_0 where T_0_0: interface_type_mangling.PP, T_0_0: interface_type_mangling.PQ> (T_0_0) -> ()
func f3<T where T: PQ, T: PP, T: P>(x: T) {}

// CHECK-LABEL: interface_type_mangling.g1 : <T_0_0, T_0_1 where T_0_0: interface_type_mangling.PP, T_0_1: interface_type_mangling.PQ> (T_0_1, y : T_0_0) -> ()
func g1<U, T where T: PQ, U: PP>(x: T, y: U) {}
// CHECK-LABEL: interface_type_mangling.g2 : <T_0_0, T_0_1 where T_0_0: interface_type_mangling.PP, T_0_1: interface_type_mangling.PQ> (T_0_1, y : T_0_0) -> ()
func g2<U, T where T: PQ, T.Assoc1: A, U: PP>(x: T, y: U) {}
// CHECK-LABEL: interface_type_mangling.g3 : <T_0_0, T_0_1 where T_0_0: interface_type_mangling.PP, T_0_1: interface_type_mangling.PQ> (T_0_1, y : T_0_0) -> ()
func g3<U, T where U: PP, T: PQ, T.Assoc1: A>(x: T, y: U) {}

// CHECK-LABEL: interface_type_mangling.h1 : <T_0_0 where T_0_0: interface_type_mangling.Base, T_0_0: interface_type_mangling.P> (T_0_0) -> ()
func h1<T where T: Base, T: P>(x: T) {}
// CHECK-LABEL: interface_type_mangling.h2 : <T_0_0 where T_0_0: interface_type_mangling.Base, T_0_0: interface_type_mangling.P> (T_0_0) -> ()
func h2<T where T: P, T: Base>(x: T) {}
// FIXME: Q and AnyObject constraints should be implied by base class constraint. rdar://problem/20829810
// FIXME-LABEL: interface_type_mangling.h3 : <T_0_0 where T_0_0: interface_type_mangling.Base, T_0_0: interface_type_mangling.P> (T_0_0) -> ()
func h3<T where T: P, T: Base, T: AnyObject>(x: T) {}
// FIXME-LABEL: interface_type_mangling.h4 : <T_0_0 where T_0_0: interface_type_mangling.Base, T_0_0: interface_type_mangling.P> (T_0_0) -> ()
func h4<T where T: P, T: Base, T: Q>(x: T) {}
// FIXME-LABEL: interface_type_mangling.h5 : <T_0_0 where T_0_0: interface_type_mangling.Base, T_0_0: interface_type_mangling.P> (T_0_0) -> ()
func h5<T where T: P, T: Base, T: Q /* TODO: same type constraints , T.Assoc0 == Base*/>(x: T) {}

// interface_type_mangling.i1 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1: interface_type_mangling.P, T_0_0.Assoc0: interface_type_mangling.Q> (T_0_0) -> ()
func i1<T where T: P, T: Q, T.Assoc1: P, T.Assoc0: Q>(x: T) {}
// interface_type_mangling.i2 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1: interface_type_mangling.P, T_0_0.Assoc0: interface_type_mangling.Q> (T_0_0) -> ()
func i2<T where T: P, T: Q, T.Assoc0: Q, T.Assoc1: P>(x: T) {}
