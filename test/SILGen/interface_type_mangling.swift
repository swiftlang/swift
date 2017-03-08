// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -Xllvm -sil-full-demangle %s -emit-silgen | %FileCheck %s

protocol P {
  associatedtype Assoc1
  associatedtype Assoc2
}
protocol PP: P {}
protocol PQ: P {
  associatedtype Assoc1: A
}
protocol Q {
  associatedtype Assoc0: A
}

protocol A {
  associatedtype Assoc
}

class Base: Q, A {
  typealias Assoc = Base
  typealias Assoc0 = Base
}

// CHECK-LABEL: interface_type_mangling.f1
// CHECK:                                  [[F_SIGNATURE:<A where A: interface_type_mangling.PP, A: interface_type_mangling.PQ> \(A\) -> \(\)]]
func f1<T where T: PP, T: PQ>(_ x: T) {}
// CHECK:       interface_type_mangling.f2 [[F_SIGNATURE]]
func f2<T where T: PQ, T: PP>(_ x: T) {}
// CHECK:       interface_type_mangling.f3 [[F_SIGNATURE]]
func f3<T where T: PQ, T: PP, T: P>(_ x: T) {}

// CHECK-LABEL: interface_type_mangling.g1
// CHECK:                                  [[G_SIGNATURE:<A, B where A: interface_type_mangling.PP, B: interface_type_mangling.PQ> \(B, y : A\) -> \(\)]]
func g1<U, T where T: PQ, U: PP>(_ x: T, y: U) {}
// CHECK:       interface_type_mangling.g2 [[G_SIGNATURE]]
func g2<U, T where T: PQ, T.Assoc1: A, U: PP>(_ x: T, y: U) {}
// CHECK:       interface_type_mangling.g3 [[G_SIGNATURE]]
func g3<U, T where U: PP, T: PQ, T.Assoc1: A>(_ x: T, y: U) {}

// CHECK-LABEL: interface_type_mangling.h1
// CHECK:                                  [[H_SIGNATURE:<A where A: interface_type_mangling.Base, A: interface_type_mangling.P> \(A\) -> \(\)]]
func h1<T where T: Base, T: P>(_ x: T) {}
// CHECK:       interface_type_mangling.h2 [[H_SIGNATURE]]
func h2<T where T: P, T: Base>(_ x: T) {}
// CHECK:       interface_type_mangling.h3 [[H_SIGNATURE]]
func h3<T where T: P, T: Base, T: AnyObject>(_ x: T) {}
// CHECK:       interface_type_mangling.h4 [[H_SIGNATURE]]
func h4<T where T: P, T: Base, T: Q>(_ x: T) {}
// CHECK:       interface_type_mangling.h5 [[H_SIGNATURE]]
func h5<T where T: P, T: Base, T: Q /* TODO: same type constraints , T.Assoc0 == Base*/>(_ x: T) {}

// CHECK-LABEL: interface_type_mangling.i1
// CHECK:                                    [[I_SIGNATURE:<A where A: interface_type_mangling.P, A: interface_type_mangling.Q, A.Assoc0: interface_type_mangling.Q, A.Assoc1: interface_type_mangling.P> \(A\) -> \(\)]]
func i1<T where T: P, T: Q, T.Assoc1: P, T.Assoc0: Q>(_ x: T) {}
// CHECK:       interface_type_mangling.i2 [[I_SIGNATURE]]
func i2<T where T: P, T: Q, T.Assoc0: Q, T.Assoc1: P>(_ x: T) {}

// CHECK-LABEL: interface_type_mangling.j01
// CHECK:                                     [[J_SIGNATURE:<A where A: interface_type_mangling.P, A: interface_type_mangling.Q, A.Assoc0 == A.Assoc1, A.Assoc1 == A.Assoc2> \(A\) -> \(\)]]
func j01<T where T: P, T: Q, T.Assoc0 == T.Assoc1, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.j02 [[J_SIGNATURE]]
func j02<T where T: P, T: Q, T.Assoc0 == T.Assoc2, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.j03 [[J_SIGNATURE]]
func j03<T where T: P, T: Q, T.Assoc0 == T.Assoc2, T.Assoc1 == T.Assoc0>(_ x: T) {}
// CHECK:       interface_type_mangling.j04 [[J_SIGNATURE]]
func j04<T where T: P, T: Q, T.Assoc1 == T.Assoc0, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.j05 [[J_SIGNATURE]]
func j05<T where T: P, T: Q, T.Assoc2 == T.Assoc0, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.j06 [[J_SIGNATURE]]
func j06<T where T: P, T: Q, T.Assoc2 == T.Assoc0, T.Assoc1 == T.Assoc0>(_ x: T) {}
// CHECK:       interface_type_mangling.j07 [[J_SIGNATURE]]
func j07<T where T: P, T: Q, T.Assoc1 == T.Assoc0, T.Assoc2 == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.j08 [[J_SIGNATURE]]
func j08<T where T: P, T: Q, T.Assoc2 == T.Assoc0, T.Assoc2 == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.j09 [[J_SIGNATURE]]
func j09<T where T: P, T: Q, T.Assoc2 == T.Assoc0, T.Assoc0 == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.j10 [[J_SIGNATURE]]
func j10<T where T: P, T: Q, T.Assoc1 == T.Assoc2, T.Assoc0 == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.j11 [[J_SIGNATURE]]
func j11<T where T: P, T: Q, T.Assoc1 == T.Assoc2, T.Assoc0 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.j12 [[J_SIGNATURE]]
func j12<T where T: P, T: Q, T.Assoc1 == T.Assoc0, T.Assoc0 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.j13 [[J_SIGNATURE]]
func j13<T where T: P, T: Q, T.Assoc1 == T.Assoc2, T.Assoc1 == T.Assoc0>(_ x: T) {}
// CHECK:       interface_type_mangling.j14 [[J_SIGNATURE]]
func j14<T where T: P, T: Q, T.Assoc1 == T.Assoc2, T.Assoc2 == T.Assoc0>(_ x: T) {}
// CHECK:       interface_type_mangling.j15 [[J_SIGNATURE]]
func j15<T where T: P, T: Q, T.Assoc1 == T.Assoc0, T.Assoc2 == T.Assoc0>(_ x: T) {}
// CHECK:       interface_type_mangling.j16 [[J_SIGNATURE]]
func j16<T where T: P, T: Q, T.Assoc2 == T.Assoc1, T.Assoc1 == T.Assoc0>(_ x: T) {}
// CHECK:       interface_type_mangling.j17 [[J_SIGNATURE]]
func j17<T where T: P, T: Q, T.Assoc2 == T.Assoc1, T.Assoc2 == T.Assoc0>(_ x: T) {}
// CHECK:       interface_type_mangling.j18 [[J_SIGNATURE]]
func j18<T where T: P, T: Q, T.Assoc0 == T.Assoc1, T.Assoc2 == T.Assoc0>(_ x: T) {}

struct S {}
struct G<X> {}

// CHECK-LABEL: interface_type_mangling.k01
// CHECK:                                     [[K_SIGNATURE:<A where A: interface_type_mangling.P, A.Assoc1 == interface_type_mangling.S, A.Assoc2 == interface_type_mangling.S> \(A\) -> \(\)]]
func k01<T where T: P, S == T.Assoc1, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.k02 [[K_SIGNATURE]]
func k02<T where T: P, S == T.Assoc2, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.k03 [[K_SIGNATURE]]
func k03<T where T: P, S == T.Assoc2, T.Assoc1 == S>(_ x: T) {}
// CHECK:       interface_type_mangling.k04 [[K_SIGNATURE]]
func k04<T where T: P, T.Assoc1 == S, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.k05 [[K_SIGNATURE]]
func k05<T where T: P, T.Assoc2 == S, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.k06 [[K_SIGNATURE]]
func k06<T where T: P, T.Assoc2 == S, T.Assoc1 == S>(_ x: T) {}
// CHECK:       interface_type_mangling.k07 [[K_SIGNATURE]]
func k07<T where T: P, T.Assoc1 == S, T.Assoc2 == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.k08 [[K_SIGNATURE]]
func k08<T where T: P, T.Assoc2 == S, T.Assoc2 == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.k09 [[K_SIGNATURE]]
func k09<T where T: P, T.Assoc2 == S, S == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.k10 [[K_SIGNATURE]]
func k10<T where T: P, T.Assoc1 == T.Assoc2, S == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.k11 [[K_SIGNATURE]]
func k11<T where T: P, T.Assoc1 == T.Assoc2, S == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.k12 [[K_SIGNATURE]]
func k12<T where T: P, T.Assoc1 == S, S == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.k13 [[K_SIGNATURE]]
func k13<T where T: P, T.Assoc1 == T.Assoc2, T.Assoc1 == S>(_ x: T) {}
// CHECK:       interface_type_mangling.k14 [[K_SIGNATURE]]
func k14<T where T: P, T.Assoc1 == T.Assoc2, T.Assoc2 == S>(_ x: T) {}
// CHECK:       interface_type_mangling.k15 [[K_SIGNATURE]]
func k15<T where T: P, T.Assoc1 == S, T.Assoc2 == S>(_ x: T) {}
// CHECK:       interface_type_mangling.k16 [[K_SIGNATURE]]
func k16<T where T: P, T.Assoc2 == T.Assoc1, T.Assoc1 == S>(_ x: T) {}
// CHECK:       interface_type_mangling.k17 [[K_SIGNATURE]]
func k17<T where T: P, T.Assoc2 == T.Assoc1, T.Assoc2 == S>(_ x: T) {}
// CHECK:       interface_type_mangling.k18 [[K_SIGNATURE]]
func k18<T where T: P, S == T.Assoc1, T.Assoc2 == S>(_ x: T) {}

// CHECK-LABEL: interface_type_mangling.L01
// CHECK:                                     [[L_SIGNATURE:<A where A: interface_type_mangling.P, A.Assoc1 == interface_type_mangling.G<A>, A.Assoc2 == interface_type_mangling.G<A>> \(A\) -> \(\)]]
func L01<T where T: P, G<T> == T.Assoc1, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.L02 [[L_SIGNATURE]]
func L02<T where T: P, G<T> == T.Assoc2, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.L03 [[L_SIGNATURE]]
func L03<T where T: P, G<T> == T.Assoc2, T.Assoc1 == G<T>>(_ x: T) {}
// CHECK:       interface_type_mangling.L04 [[L_SIGNATURE]]
func L04<T where T: P, T.Assoc1 == G<T>, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.L05 [[L_SIGNATURE]]
func L05<T where T: P, T.Assoc2 == G<T>, T.Assoc1 == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.L06 [[L_SIGNATURE]]
func L06<T where T: P, T.Assoc2 == G<T>, T.Assoc1 == G<T>>(_ x: T) {}
// CHECK:       interface_type_mangling.L07 [[L_SIGNATURE]]
func L07<T where T: P, T.Assoc1 == G<T>, T.Assoc2 == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.L08 [[L_SIGNATURE]]
func L08<T where T: P, T.Assoc2 == G<T>, T.Assoc2 == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.L09 [[L_SIGNATURE]]
func L09<T where T: P, T.Assoc2 == G<T>, G<T> == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.L10 [[L_SIGNATURE]]
func L10<T where T: P, T.Assoc1 == T.Assoc2, G<T> == T.Assoc1>(_ x: T) {}
// CHECK:       interface_type_mangling.L11 [[L_SIGNATURE]]
func L11<T where T: P, T.Assoc1 == T.Assoc2, G<T> == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.L12 [[L_SIGNATURE]]
func L12<T where T: P, T.Assoc1 == G<T>, G<T> == T.Assoc2>(_ x: T) {}
// CHECK:       interface_type_mangling.L13 [[L_SIGNATURE]]
func L13<T where T: P, T.Assoc1 == T.Assoc2, T.Assoc1 == G<T>>(_ x: T) {}
// CHECK:       interface_type_mangling.L14 [[L_SIGNATURE]]
func L14<T where T: P, T.Assoc1 == T.Assoc2, T.Assoc2 == G<T>>(_ x: T) {}
// CHECK:       interface_type_mangling.L15 [[L_SIGNATURE]]
func L15<T where T: P, T.Assoc1 == G<T>, T.Assoc2 == G<T>>(_ x: T) {}
// CHECK:       interface_type_mangling.L16 [[L_SIGNATURE]]
func L16<T where T: P, T.Assoc2 == T.Assoc1, T.Assoc1 == G<T>>(_ x: T) {}
// CHECK:       interface_type_mangling.L17 [[L_SIGNATURE]]
func L17<T where T: P, T.Assoc2 == T.Assoc1, T.Assoc2 == G<T>>(_ x: T) {}
// CHECK:       interface_type_mangling.L18 [[L_SIGNATURE]]
func L18<T where T: P, G<T> == T.Assoc1, T.Assoc2 == G<T>>(_ x: T) {}

struct X {}; struct Y {}

// CHECK-LABEL: interface_type_mangling.m1
// CHECK:                                    [[M_SIGNATURE:<A, B where A: interface_type_mangling.A, B: interface_type_mangling.A, A.Assoc == interface_type_mangling.X, B.Assoc == interface_type_mangling.Y> \(A, y : B\) -> \(\)]]
func m1<T: A, U: A where T.Assoc == X, U.Assoc == Y>(_ x: T, y: U) {}
// CHECK:       interface_type_mangling.m2 [[M_SIGNATURE]]
func m2<T: A, U: A where U.Assoc == Y, T.Assoc == X>(_ x: T, y: U) {}
// CHECK:       interface_type_mangling.m3 [[M_SIGNATURE]]
func m3<T, U where T: A, U: A, U.Assoc == Y, T.Assoc == X>(_ x: T, y: U) {}

protocol GenericWitnessTest {
  associatedtype Tee

  func closureInGenericContext<X>(_ b: X)
  var closureInGenericPropertyContext: Tee { get }
  func twoParamsAtDepth<Y, Z>(_ x: Y, y: Z)
}

struct GenericTypeContext<T>: GenericWitnessTest {
  typealias Tee = T

  var a: T
  // CHECK-LABEL: sil shared @_T023interface_type_mangling18GenericTypeContextV09closureIndF0yqd__lF3fooL_yx_qd__tr__lF
  func closureInGenericContext<U>(_ b: U) {
    func foo(_ x: T, _ y: U) { }

    foo(a, b)
  }

  // CHECK-LABEL: sil shared @_T023interface_type_mangling18GenericTypeContextV09closureInd8PropertyF0xfg3fooL_xylF
  var closureInGenericPropertyContext: T {
    func foo() -> T { }

    return foo()
  }

  // FIXME: Demangling for generic params at depth is wrong.
  // CHECK-LABEL: twoParamsAtDepth <A, B> (A1, y : B1) -> ()
  // CHECK-LABEL: sil hidden @_T023interface_type_mangling18GenericTypeContextV16twoParamsAtDepthyqd___qd_0_1ytr0_lF
  func twoParamsAtDepth<A, B>(_ x: A, y: B) {}
}

// CHECK-LABEL: protocol witness for interface_type_mangling.GenericWitnessTest.closureInGenericContext <A> (A1) -> () in conformance <A> interface_type_mangling.GenericTypeContext<A> : interface_type_mangling.GenericWitnessTest in interface_type_mangling
// CHECK-LABEL: @_T023interface_type_mangling18GenericTypeContextVyxGAA0D11WitnessTestAAlAaEP09closureIndF0yqd__lFTW

// CHECK-LABEL: protocol witness for interface_type_mangling.GenericWitnessTest.closureInGenericPropertyContext.getter : A.Tee in conformance <A> interface_type_mangling.GenericTypeContext<A> : interface_type_mangling.GenericWitnessTest in interface_type_mangling
// CHECK-LABEL: @_T023interface_type_mangling18GenericTypeContextVyxGAA0D11WitnessTestAAlAaEP09closureInd8PropertyF03TeeQzfgTW

// CHECK-LABEL: protocol witness for interface_type_mangling.GenericWitnessTest.twoParamsAtDepth <A, B> (A1, y : B1) -> () in conformance <A> interface_type_mangling.GenericTypeContext<A> : interface_type_mangling.GenericWitnessTest in interface_type_mangling
// CHECK-LABEL: @_T023interface_type_mangling18GenericTypeContextVyxGAA0D11WitnessTestAAlAaEP16twoParamsAtDepthyqd___qd_0_1ytr0_lFTW
