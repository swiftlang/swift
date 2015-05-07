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

/* FIXME: ArchetypeBuilder introduces extra associated type equivalence
 * classes without filtering them out as redundant. */
// CHECK-LABEL: interface_type_mangling.j01 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j01<T where T: P, T: Q, T.Assoc0 == T.Assoc1, T.Assoc1 == T.Assoc2>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j02 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j02<T where T: P, T: Q, T.Assoc0 == T.Assoc2, T.Assoc1 == T.Assoc2>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j03 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j03<T where T: P, T: Q, T.Assoc0 == T.Assoc2, T.Assoc1 == T.Assoc0>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j04 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j04<T where T: P, T: Q, T.Assoc1 == T.Assoc0, T.Assoc1 == T.Assoc2>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j05 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j05<T where T: P, T: Q, T.Assoc2 == T.Assoc0, T.Assoc1 == T.Assoc2>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j06 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j06<T where T: P, T: Q, T.Assoc2 == T.Assoc0, T.Assoc1 == T.Assoc0>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j07 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j07<T where T: P, T: Q, T.Assoc1 == T.Assoc0, T.Assoc2 == T.Assoc1>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j08 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j08<T where T: P, T: Q, T.Assoc2 == T.Assoc0, T.Assoc2 == T.Assoc1>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j09 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j09<T where T: P, T: Q, T.Assoc2 == T.Assoc0, T.Assoc0 == T.Assoc1>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j10 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j10<T where T: P, T: Q, T.Assoc1 == T.Assoc2, T.Assoc0 == T.Assoc1>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j11 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j11<T where T: P, T: Q, T.Assoc1 == T.Assoc2, T.Assoc0 == T.Assoc2>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j12 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j12<T where T: P, T: Q, T.Assoc1 == T.Assoc0, T.Assoc0 == T.Assoc2>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j13 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j13<T where T: P, T: Q, T.Assoc1 == T.Assoc2, T.Assoc1 == T.Assoc0>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j14 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j14<T where T: P, T: Q, T.Assoc1 == T.Assoc2, T.Assoc2 == T.Assoc0>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j15 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j15<T where T: P, T: Q, T.Assoc1 == T.Assoc0, T.Assoc2 == T.Assoc0>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j16 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j16<T where T: P, T: Q, T.Assoc2 == T.Assoc1, T.Assoc1 == T.Assoc0>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j17 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j17<T where T: P, T: Q, T.Assoc2 == T.Assoc1, T.Assoc2 == T.Assoc0>(x: T) {}
// FIXME-LABEL: interface_type_mangling.j18 : <T_0_0 where T_0_0: interface_type_mangling.P, T_0_0: interface_type_mangling.Q, T_0_0.Assoc1 == T_0_0.Assoc0, T_0_0.Assoc2 == T_0_0.Assoc0> (T_0_0) -> ()
func j18<T where T: P, T: Q, T.Assoc0 == T.Assoc1, T.Assoc2 == T.Assoc0>(x: T) {}

struct S {}
struct G<X> {}

/* FIXME: ArchetypeBuilder doesn't consistently group same-type equivalence
 * classes with concrete types. */
func k01<T where T: P, S == T.Assoc1, T.Assoc1 == T.Assoc2>(x: T) {}
func k02<T where T: P, S == T.Assoc2, T.Assoc1 == T.Assoc2>(x: T) {}
func k03<T where T: P, S == T.Assoc2, T.Assoc1 == S>(x: T) {}
func k04<T where T: P, T.Assoc1 == S, T.Assoc1 == T.Assoc2>(x: T) {}
func k05<T where T: P, T.Assoc2 == S, T.Assoc1 == T.Assoc2>(x: T) {}
func k06<T where T: P, T.Assoc2 == S, T.Assoc1 == S>(x: T) {}
func k07<T where T: P, T.Assoc1 == S, T.Assoc2 == T.Assoc1>(x: T) {}
func k08<T where T: P, T.Assoc2 == S, T.Assoc2 == T.Assoc1>(x: T) {}
func k09<T where T: P, T.Assoc2 == S, S == T.Assoc1>(x: T) {}
func k10<T where T: P, T.Assoc1 == T.Assoc2, S == T.Assoc1>(x: T) {}
func k11<T where T: P, T.Assoc1 == T.Assoc2, S == T.Assoc2>(x: T) {}
func k12<T where T: P, T.Assoc1 == S, S == T.Assoc2>(x: T) {}
func k13<T where T: P, T.Assoc1 == T.Assoc2, T.Assoc1 == S>(x: T) {}
func k14<T where T: P, T.Assoc1 == T.Assoc2, T.Assoc2 == S>(x: T) {}
func k15<T where T: P, T.Assoc1 == S, T.Assoc2 == S>(x: T) {}
func k16<T where T: P, T.Assoc2 == T.Assoc1, T.Assoc1 == S>(x: T) {}
func k17<T where T: P, T.Assoc2 == T.Assoc1, T.Assoc2 == S>(x: T) {}
func k18<T where T: P, S == T.Assoc1, T.Assoc2 == S>(x: T) {}

/* FIXME: ArchetypeBuilder's interface leaves archetypes in the same-type
 * constraints, causing mangling to crash on these.
func L01<T where T: P, G<T> == T.Assoc1, T.Assoc1 == T.Assoc2>(x: T) {}
func L02<T where T: P, G<T> == T.Assoc2, T.Assoc1 == T.Assoc2>(x: T) {}
func L03<T where T: P, G<T> == T.Assoc2, T.Assoc1 == G<T>>(x: T) {}
func L04<T where T: P, T.Assoc1 == G<T>, T.Assoc1 == T.Assoc2>(x: T) {}
func L05<T where T: P, T.Assoc2 == G<T>, T.Assoc1 == T.Assoc2>(x: T) {}
func L06<T where T: P, T.Assoc2 == G<T>, T.Assoc1 == G<T>>(x: T) {}
func L07<T where T: P, T.Assoc1 == G<T>, T.Assoc2 == T.Assoc1>(x: T) {}
func L08<T where T: P, T.Assoc2 == G<T>, T.Assoc2 == T.Assoc1>(x: T) {}
func L09<T where T: P, T.Assoc2 == G<T>, G<T> == T.Assoc1>(x: T) {}
func L10<T where T: P, T.Assoc1 == T.Assoc2, G<T> == T.Assoc1>(x: T) {}
func L11<T where T: P, T.Assoc1 == T.Assoc2, G<T> == T.Assoc2>(x: T) {}
func L12<T where T: P, T.Assoc1 == G<T>, G<T> == T.Assoc2>(x: T) {}
func L13<T where T: P, T.Assoc1 == T.Assoc2, T.Assoc1 == G<T>>(x: T) {}
func L14<T where T: P, T.Assoc1 == T.Assoc2, T.Assoc2 == G<T>>(x: T) {}
func L15<T where T: P, T.Assoc1 == G<T>, T.Assoc2 == G<T>>(x: T) {}
func L16<T where T: P, T.Assoc2 == T.Assoc1, T.Assoc1 == G<T>>(x: T) {}
func L17<T where T: P, T.Assoc2 == T.Assoc1, T.Assoc2 == G<T>>(x: T) {}
func L18<T where T: P, G<T> == T.Assoc1, T.Assoc2 == G<T>>(x: T) {}
 */
