// RUN: %swift -emit-sil-protocol-witness-tables -emit-silgen -parse-stdlib %s | FileCheck %s

operator infix <~> {}

struct Loadable {}
protocol AddrOnly {}
@class_protocol protocol Classes {}

protocol X {
  func selfTypes(x: Self) -> Self
  func loadable(x: Loadable) -> Loadable
  func addrOnly(x: AddrOnly) -> AddrOnly
  func generic<A>(x: A) -> A
  func classes<A2: Classes>(x: A2) -> A2
  func <~>(x: Self, y: Self) -> Self
}
protocol Y {}

@class_protocol protocol ClassBounded {
  func selfTypes(x: Self) -> Self
}

struct ConformingStruct : X {
  func selfTypes(x: ConformingStruct) -> ConformingStruct { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses16ConformingStructS_1XS_S1_9selfTypesU_fRQPS1_FT1xS2__S2_ : $@cc(method) @thin (@out ConformingStruct, @in ConformingStruct, @inout ConformingStruct) -> () {
  // CHECK-NEXT:  bb0(%0 : $*ConformingStruct, %1 : $*ConformingStruct, %2 : $*ConformingStruct):
  // CHECK-NEXT:    %3 = load %1 : $*ConformingStruct
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %4 = function_ref @_TV9witnesses16ConformingStruct9selfTypesfRS0_FT1xS0__S0_ : $@cc(method) @thin (ConformingStruct, @inout ConformingStruct) -> ConformingStruct
  // CHECK-NEXT:    %5 = apply %4(%3, %2) : $@cc(method) @thin (ConformingStruct, @inout ConformingStruct) -> ConformingStruct
  // CHECK-NEXT:    store %5 to %0 : $*ConformingStruct
  // CHECK-NEXT:    %7 = tuple ()
  // CHECK-NEXT:    return %7 : $()
  // CHECK-NEXT:  }
  func loadable(x: Loadable) -> Loadable { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses16ConformingStructS_1XS_S1_8loadableU_fRQPS1_FT1xVS_8Loadable_S3_ : $@cc(method) @thin (Loadable, @inout ConformingStruct) -> Loadable {
  // CHECK-NEXT:  bb0(%0 : $Loadable, %1 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %2 = function_ref @_TV9witnesses16ConformingStruct8loadablefRS0_FT1xVS_8Loadable_S1_ : $@cc(method) @thin (Loadable, @inout ConformingStruct) -> Loadable
  // CHECK-NEXT:    %3 = apply %2(%0, %1) : $@cc(method) @thin (Loadable, @inout ConformingStruct) -> Loadable
  // CHECK-NEXT:    return %3 : $Loadable
  // CHECK-NEXT:  }
  func addrOnly(x: AddrOnly) -> AddrOnly { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses16ConformingStructS_1XS_S1_8addrOnlyU_fRQPS1_FT1xPS_8AddrOnly__PS3__ : $@cc(method) @thin (@out AddrOnly, @in AddrOnly, @inout ConformingStruct) -> () {
  // CHECK-NEXT:  bb0(%0 : $*AddrOnly, %1 : $*AddrOnly, %2 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TV9witnesses16ConformingStruct8addrOnlyfRS0_FT1xPS_8AddrOnly__PS1__ : $@cc(method) @thin (@out AddrOnly, @in AddrOnly, @inout ConformingStruct) -> ()
  // CHECK-NEXT:    %4 = apply %3(%0, %1, %2) : $@cc(method) @thin (@out AddrOnly, @in AddrOnly, @inout ConformingStruct) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }
  func generic<C>(x: C) -> C { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses16ConformingStructS_1XS_S1_7genericU_fRQPS1_U__FT1xQ__Q_ : $@cc(method) @thin <A> (@out A, @in A, @inout ConformingStruct) -> () {
  // CHECK-NEXT:  bb0(%0 : $*A, %1 : $*A, %2 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TV9witnesses16ConformingStruct7genericfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <C> (@out C, @in C, @inout ConformingStruct) -> ()
  // CHECK-NEXT:    %4 = apply %3<C = A>(%0, %1, %2) : $@cc(method) @thin <C> (@out C, @in C, @inout ConformingStruct) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }
  func classes<C2: Classes>(x: C2) -> C2 { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses16ConformingStructS_1XS_S1_7classesU_fRQPS1_US_7Classes__FT1xQ__Q_ : $@cc(method) @thin <A2 : Classes> (@owned A2, @inout ConformingStruct) -> @owned A2 {
  // CHECK-NEXT:  bb0(%0 : $A2, %1 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %2 = function_ref @_TV9witnesses16ConformingStruct7classesfRS0_US_7Classes__FT1xQ__Q_ : $@cc(method) @thin <C2 : Classes> (@owned C2, @inout ConformingStruct) -> @owned C2
  // CHECK-NEXT:    %3 = apply %2<C2 = A2>(%0, %1) : $@cc(method) @thin <C2 : Classes> (@owned C2, @inout ConformingStruct) -> @owned C2
  // CHECK-NEXT:    return %3 : $A2
  // CHECK-NEXT:  }
}
func <~>(x: ConformingStruct, y: ConformingStruct) -> ConformingStruct { return x }
// CHECK-LABEL: sil internal @_TTWV9witnesses16ConformingStructS_1XS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__S2_ : $@thin (@out ConformingStruct, @in ConformingStruct, @in ConformingStruct, ConformingStruct.metatype) -> () {
// CHECK-NEXT:  bb0(%0 : $*ConformingStruct, %1 : $*ConformingStruct, %2 : $*ConformingStruct, %3 : $ConformingStruct.metatype):
// CHECK-NEXT:    %4 = load %1 : $*ConformingStruct
// CHECK-NEXT:    %5 = load %2 : $*ConformingStruct
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    %6 = function_ref @_T9witnessesoi3ltgFT1xVS_16ConformingStruct1yS0__S0_ : $@thin (ConformingStruct, ConformingStruct) -> ConformingStruct
// CHECK-NEXT:    %7 = apply %6(%4, %5) : $@thin (ConformingStruct, ConformingStruct) -> ConformingStruct
// CHECK-NEXT:    store %7 to %0 : $*ConformingStruct
// CHECK-NEXT:    %9 = tuple ()
// CHECK-NEXT:    return %9 : $()
// CHECK-NEXT:  }

class ConformingClass : X {
  func selfTypes(x: ConformingClass) -> ConformingClass { return x }
  // CHECK-LABEL: sil internal @_TTWC9witnesses15ConformingClassS_1XS_S1_9selfTypesU_fRQPS1_FT1xS2__S2_ : $@cc(method) @thin (@out ConformingClass, @in ConformingClass, @inout ConformingClass) -> () {
  // CHECK-NEXT:  bb0(%0 : $*ConformingClass, %1 : $*ConformingClass, %2 : $*ConformingClass):
  // -- load and retain 'self' from inout witness 'self' parameter
  // CHECK-NEXT:    %3 = load %2 : $*ConformingClass
  // CHECK-NEXT:    strong_retain %3 : $ConformingClass
  // CHECK-NEXT:    %5 = load %1 : $*ConformingClass
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %6 = function_ref @_TC9witnesses15ConformingClass9selfTypesfS0_FT1xS0__S0_ : $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass
  // CHECK-NEXT:    %7 = apply %6(%5, %3) : $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass
  // CHECK-NEXT:    store %7 to %0 : $*ConformingClass
  // CHECK-NEXT:    %9 = tuple ()
  // CHECK-NEXT:    return %9 : $()
  // CHECK-NEXT:  }
  func loadable(x: Loadable) -> Loadable { return x }
  func addrOnly(x: AddrOnly) -> AddrOnly { return x }
  func generic<D>(x: D) -> D { return x }
  func classes<D2: Classes>(x: D2) -> D2 { return x }
}
func <~>(x: ConformingClass, y: ConformingClass) -> ConformingClass { return x }

extension ConformingClass : ClassBounded { }
// CHECK-LABEL: sil internal @_TTWC9witnesses15ConformingClassS_12ClassBoundedS_S1_9selfTypesU_fQPS1_FT1xS2__S2_ : $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass {
// CHECK-NEXT:  bb0(%0 : $ConformingClass, %1 : $ConformingClass):
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    %2 = function_ref @_TC9witnesses15ConformingClass9selfTypesfS0_FT1xS0__S0_ : $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass
// CHECK-NEXT:    %3 = apply %2(%0, %1) : $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass
// CHECK-NEXT:    return %3 : $ConformingClass
// CHECK-NEXT:  }

struct ConformingAOStruct : X {
  var makeMeAO : AddrOnly

  func selfTypes(x: ConformingAOStruct) -> ConformingAOStruct { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses18ConformingAOStructS_1XS_S1_9selfTypesU_fRQPS1_FT1xS2__S2_ : $@cc(method) @thin (@out ConformingAOStruct, @in ConformingAOStruct, @inout ConformingAOStruct) -> () {
  // CHECK-NEXT:  bb0(%0 : $*ConformingAOStruct, %1 : $*ConformingAOStruct, %2 : $*ConformingAOStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TV9witnesses18ConformingAOStruct9selfTypesfRS0_FT1xS0__S0_ : $@cc(method) @thin (@out ConformingAOStruct, @in ConformingAOStruct, @inout ConformingAOStruct) -> ()
  // CHECK-NEXT:    %4 = apply %3(%0, %1, %2) : $@cc(method) @thin (@out ConformingAOStruct, @in ConformingAOStruct, @inout ConformingAOStruct) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }
  func loadable(x: Loadable) -> Loadable { return x }
  func addrOnly(x: AddrOnly) -> AddrOnly { return x }
  func generic<D>(x: D) -> D { return x }
  func classes<D2: Classes>(x: D2) -> D2 { return x }
}
func <~>(x: ConformingAOStruct, y: ConformingAOStruct) -> ConformingAOStruct { return x }

// TODO: The extra abstraction change in these generic witnesses leads to a
// bunch of redundant temporaries that could be avoided by a more sophisticated
// abstraction difference implementation.

struct ConformsWithMoreGeneric : X, Y {
  func selfTypes<E>(x: E) -> E { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_S1_9selfTypesU_fRQPS1_FT1xS2__S2_ : $@cc(method) @thin (@out ConformsWithMoreGeneric, @in ConformsWithMoreGeneric, @inout ConformsWithMoreGeneric) -> () {
  // CHECK-NEXT:  bb0(%0 : $*ConformsWithMoreGeneric, %1 : $*ConformsWithMoreGeneric, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    %3 = load %1 : $*ConformsWithMoreGeneric
  // CHECK-NEXT:    %4 = alloc_stack $ConformsWithMoreGeneric
  // CHECK-NEXT:    store %3 to %4#1 : $*ConformsWithMoreGeneric
  // CHECK-NEXT:    %6 = alloc_stack $ConformsWithMoreGeneric
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %7 = function_ref @_TV9witnesses23ConformsWithMoreGeneric9selfTypesfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <E> (@out E, @in E, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %8 = apply %7<E = ConformsWithMoreGeneric>(%6#1, %4#1, %2) : $@cc(method) @thin <E> (@out E, @in E, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %9 = load %6#1 : $*ConformsWithMoreGeneric
  // CHECK-NEXT:    store %9 to %0 : $*ConformsWithMoreGeneric
  // CHECK-NEXT:    %11 = tuple ()
  // CHECK-NEXT:    dealloc_stack %6#0 : $*@local_storage ConformsWithMoreGeneric
  // CHECK-NEXT:    dealloc_stack %4#0 : $*@local_storage ConformsWithMoreGeneric
  // CHECK-NEXT:    return %11 : $()
  // CHECK-NEXT:  }
  func loadable<F>(x: F) -> F { return x }
  func addrOnly<G>(x: G) -> G { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_S1_8addrOnlyU_fRQPS1_FT1xPS_8AddrOnly__PS3__ : $@cc(method) @thin (@out AddrOnly, @in AddrOnly, @inout ConformsWithMoreGeneric) -> () {
  // CHECK-NEXT:  bb0(%0 : $*AddrOnly, %1 : $*AddrOnly, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TV9witnesses23ConformsWithMoreGeneric8addrOnlyfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <G> (@out G, @in G, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %4 = apply %3<G = AddrOnly>(%0, %1, %2) : $@cc(method) @thin <G> (@out G, @in G, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }
  func generic<H>(x: H) -> H { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_S1_7genericU_fRQPS1_U__FT1xQ__Q_ : $@cc(method) @thin <A> (@out A, @in A, @inout ConformsWithMoreGeneric) -> () {
  // CHECK-NEXT:  bb0(%0 : $*A, %1 : $*A, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TV9witnesses23ConformsWithMoreGeneric7genericfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <H> (@out H, @in H, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %4 = apply %3<H = A>(%0, %1, %2) : $@cc(method) @thin <H> (@out H, @in H, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }
  func classes<I>(x: I) -> I { return x }
  // CHECK-LABEL: sil internal @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_S1_7classesU_fRQPS1_US_7Classes__FT1xQ__Q_ : $@cc(method) @thin <A2 : Classes> (@owned A2, @inout ConformsWithMoreGeneric) -> @owned A2 {
  // CHECK-NEXT:  bb0(%0 : $A2, %1 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    %2 = alloc_stack $A2
  // CHECK-NEXT:    store %0 to %2#1 : $*A2
  // CHECK-NEXT:    %4 = alloc_stack $A2
  // CHECK-NEXT:    // function_ref witnesses.ConformsWithMoreGeneric.classes (@inout witnesses.ConformsWithMoreGeneric)<A>(x : A) -> A
  // CHECK-NEXT:    %5 = function_ref @_TV9witnesses23ConformsWithMoreGeneric7classesfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <I> (@out I, @in I, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %6 = apply %5<I = A2>(%4#1, %2#1, %1) : $@cc(method) @thin <I> (@out I, @in I, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %7 = load %4#1 : $*A2
  // CHECK-NEXT:    dealloc_stack %4#0 : $*@local_storage A2
  // CHECK-NEXT:    dealloc_stack %2#0 : $*@local_storage A2
  // CHECK-NEXT:    return %7 : $A2
  // CHECK-NEXT:  }
}
func <~> <J: Y, K: Y>(x: J, y: K) -> K { return y }
// CHECK-LABEL: sil internal @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__S2_ : $@thin (@out ConformsWithMoreGeneric, @in ConformsWithMoreGeneric, @in ConformsWithMoreGeneric, ConformsWithMoreGeneric.metatype) -> () {
// CHECK-NEXT:  bb0(%0 : $*ConformsWithMoreGeneric, %1 : $*ConformsWithMoreGeneric, %2 : $*ConformsWithMoreGeneric, %3 : $ConformsWithMoreGeneric.metatype):
// CHECK-NEXT:    %4 = load %1 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %5 = load %2 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %6 = alloc_stack $ConformsWithMoreGeneric
// CHECK-NEXT:    store %4 to %6#1 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %8 = alloc_stack $ConformsWithMoreGeneric
// CHECK-NEXT:    store %5 to %8#1 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %10 = alloc_stack $ConformsWithMoreGeneric
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    %11 = function_ref @_T9witnessesoi3ltgUS_1Y_S0___FT1xQ_1yQ0__Q0_ : $@thin <J : Y, K : Y> (@out K, @in J, @in K) -> ()
// CHECK-NEXT:    %12 = apply %11<J = ConformsWithMoreGeneric, K = ConformsWithMoreGeneric>(%10#1, %6#1, %8#1) : $@thin <J : Y, K : Y> (@out K, @in J, @in K) -> ()
// CHECK-NEXT:    %13 = load %10#1 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    store %13 to %0 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %15 = tuple ()
// CHECK-NEXT:    dealloc_stack %10#0 : $*@local_storage ConformsWithMoreGeneric
// CHECK-NEXT:    dealloc_stack %8#0 : $*@local_storage ConformsWithMoreGeneric
// CHECK-NEXT:    dealloc_stack %6#0 : $*@local_storage ConformsWithMoreGeneric
// CHECK-NEXT:    return %15 : $()
// CHECK-NEXT:  }
