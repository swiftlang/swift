// RUN: %swift -emit-silgen %s | FileCheck %s

operator infix <~> {}

func archetype_method<T: X>(var #x: T, var #y: T) -> T {
  return x.selfTypes(x: y)
}
// CHECK-LABEL: sil @_TF9witnesses16archetype_methodUS_1X__FT1xQ_1yQ__Q_ : $@thin <T where T : X> (@out T, @in T, @in T) -> () {
// CHECK:         [[METHOD:%.*]] = witness_method $T, #X.selfTypes!1 : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : X> (@out τ_0_0, @in τ_0_0, @inout τ_0_0) -> ()
// CHECK:         apply [[METHOD]]<T>({{%.*}}, {{%.*}}, {{%.*}}) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : X> (@out τ_0_0, @in τ_0_0, @inout τ_0_0) -> ()
// CHECK:       }

func archetype_generic_method<T: X>(var #x: T, #y: Loadable) -> Loadable {
  return x.generic(x: y)
}
// CHECK-LABEL: sil @_TF9witnesses24archetype_generic_methodUS_1X__FT1xQ_1yVS_8Loadable_S1_ : $@thin <T where T : X> (@in T, Loadable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = witness_method $T, #X.generic!1 : $@cc(witness_method) @thin <τ_0_0, τ_1_0 where τ_0_0 : X> (@out τ_1_0, @in τ_1_0, @inout τ_0_0) -> ()
// CHECK:         apply [[METHOD]]<T, Loadable>({{%.*}}, {{%.*}}, {{%.*}}) : $@cc(witness_method) @thin <τ_0_0, τ_1_0 where τ_0_0 : X> (@out τ_1_0, @in τ_1_0, @inout τ_0_0) -> ()
// CHECK:       }

// CHECK-LABEL: sil @_TF9witnesses32archetype_associated_type_methodUS_13WithAssocType_U__FT1xQ_1yQQ_9AssocType_Q_ : $@thin <T where T : WithAssocType> (@out T, @in T, @in T.AssocType) -> ()
// CHECK:         apply %{{[0-9]+}}<T, T.AssocType>
func archetype_associated_type_method<T: WithAssocType>(#x: T, #y: T.AssocType) -> T {
  return x.useAssocType(x: y)
}

protocol StaticMethod { class func staticMethod() }

// CHECK-LABEL: sil @_TF9witnesses23archetype_static_methodUS_12StaticMethod__FT1xQ__T_ : $@thin <T where T : StaticMethod> (@in T) -> ()
func archetype_static_method<T: StaticMethod>(#x: T) {
  // CHECK: [[METHOD:%.*]] = witness_method $T, #StaticMethod.staticMethod!1 : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : StaticMethod> (@thick τ_0_0.Type) -> ()
  // CHECK: apply [[METHOD]]<T>
  T.staticMethod()
}

protocol Existentiable {
  func foo() -> Loadable
  func generic<T>() -> T
}

func protocol_method(#x: Existentiable) -> Loadable {
  return x.foo()
}
// CHECK-LABEL: sil @_TF9witnesses15protocol_methodFT1xPS_13Existentiable__VS_8Loadable : $@thin (@in Existentiable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = protocol_method {{%.*}} : $*Existentiable, #Existentiable.foo!1 : <`Self` : Existentiable> inout Self -> () -> Loadable, $@cc(witness_method) @callee_owned (@inout Self) -> Loadable
// CHECK:         apply [[METHOD]]({{%.*}}) : $@cc(witness_method) @callee_owned (@inout Self) -> Loadable
// CHECK:       }

func protocol_generic_method(#x: Existentiable) -> Loadable {
  return x.generic()
}
// CHECK-LABEL: sil @_TF9witnesses23protocol_generic_methodFT1xPS_13Existentiable__VS_8Loadable : $@thin (@in Existentiable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = protocol_method {{%.*}} : $*Existentiable, #Existentiable.generic!1 : <`Self` : Existentiable> inout Self -> <T> () -> T, $@cc(witness_method) @callee_owned <τ_1_0> (@out τ_1_0, @inout Self) -> ()
// CHECK:         apply [[METHOD]]<Loadable>({{%.*}}, {{%.*}}) : $@cc(witness_method) @callee_owned <τ_1_0> (@out τ_1_0, @inout Self) -> ()
// CHECK:       }

@class_protocol @objc protocol ObjCAble {
  func foo()
}

// CHECK-LABEL: sil @_TF9witnesses20protocol_objc_methodFT1xPS_8ObjCAble__T_ : $@thin (@owned ObjCAble) -> ()
// CHECK:         protocol_method [volatile] %{{[0-9]+}} : $ObjCAble, #ObjCAble.foo!1.foreign : <`Self` : ObjCAble> Self -> () -> (), $@cc(objc_method) @thin (Self) -> ()
func protocol_objc_method(#x: ObjCAble) {
  x.foo()
}

struct Loadable {}
protocol AddrOnly {}
@class_protocol protocol Classes {}

protocol X {
  mutating
  func selfTypes(#x: Self) -> Self
  mutating
  func loadable(#x: Loadable) -> Loadable
  mutating
  func addrOnly(#x: AddrOnly) -> AddrOnly
  mutating
  func generic<A>(#x: A) -> A
  mutating
  func classes<A2: Classes>(#x: A2) -> A2
  func <~>(x: Self, y: Self) -> Self
}
protocol Y {}

protocol WithAssocType {
  typealias AssocType
  func useAssocType(#x: AssocType) -> Self
}

@class_protocol protocol ClassBounded {
  func selfTypes(#x: Self) -> Self
}

struct ConformingStruct : X {
  mutating
  func selfTypes(#x: ConformingStruct) -> ConformingStruct { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses16ConformingStructS_1XFS1_9selfTypesUS1___fRQPS1_FT1xS2__S2_ : $@cc(witness_method) @thin (@out ConformingStruct, @in ConformingStruct, @inout ConformingStruct) -> () {
  // CHECK-NEXT:  bb0(%0 : $*ConformingStruct, %1 : $*ConformingStruct, %2 : $*ConformingStruct):
  // CHECK-NEXT:    %3 = load %1 : $*ConformingStruct
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %4 = function_ref @_TFV9witnesses16ConformingStruct9selfTypesfRS0_FT1xS0__S0_ : $@cc(method) @thin (ConformingStruct, @inout ConformingStruct) -> ConformingStruct
  // CHECK-NEXT:    %5 = apply %4(%3, %2) : $@cc(method) @thin (ConformingStruct, @inout ConformingStruct) -> ConformingStruct
  // CHECK-NEXT:    store %5 to %0 : $*ConformingStruct
  // CHECK-NEXT:    %7 = tuple ()
  // CHECK-NEXT:    return %7 : $()
  // CHECK-NEXT:  }
  
  mutating
  func loadable(#x: Loadable) -> Loadable { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses16ConformingStructS_1XFS1_8loadableUS1___fRQPS1_FT1xVS_8Loadable_S3_ : $@cc(witness_method) @thin (Loadable, @inout ConformingStruct) -> Loadable {
  // CHECK-NEXT:  bb0(%0 : $Loadable, %1 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %2 = function_ref @_TFV9witnesses16ConformingStruct8loadablefRS0_FT1xVS_8Loadable_S1_ : $@cc(method) @thin (Loadable, @inout ConformingStruct) -> Loadable
  // CHECK-NEXT:    %3 = apply %2(%0, %1) : $@cc(method) @thin (Loadable, @inout ConformingStruct) -> Loadable
  // CHECK-NEXT:    return %3 : $Loadable
  // CHECK-NEXT:  }
  
  mutating
  func addrOnly(#x: AddrOnly) -> AddrOnly { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses16ConformingStructS_1XFS1_8addrOnlyUS1___fRQPS1_FT1xPS_8AddrOnly__PS3__ : $@cc(witness_method) @thin (@out AddrOnly, @in AddrOnly, @inout ConformingStruct) -> () {
  // CHECK-NEXT:  bb0(%0 : $*AddrOnly, %1 : $*AddrOnly, %2 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses16ConformingStruct8addrOnlyfRS0_FT1xPS_8AddrOnly__PS1__ : $@cc(method) @thin (@out AddrOnly, @in AddrOnly, @inout ConformingStruct) -> ()
  // CHECK-NEXT:    %4 = apply %3(%0, %1, %2) : $@cc(method) @thin (@out AddrOnly, @in AddrOnly, @inout ConformingStruct) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }
  
  mutating
  func generic<C>(#x: C) -> C { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses16ConformingStructS_1XFS1_7genericUS1___fRQPS1_U__FT1xQ__Q_ : $@cc(witness_method) @thin <A> (@out A, @in A, @inout ConformingStruct) -> () {
  // CHECK-NEXT:  bb0(%0 : $*A, %1 : $*A, %2 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses16ConformingStruct7genericfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformingStruct) -> ()
  // CHECK-NEXT:    %4 = apply %3<A>(%0, %1, %2) : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformingStruct) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }
  mutating
  func classes<C2: Classes>(#x: C2) -> C2 { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses16ConformingStructS_1XFS1_7classesUS1___fRQPS1_US_7Classes__FT1xQ__Q_ : $@cc(witness_method) @thin <A2 where A2 : Classes> (@owned A2, @inout ConformingStruct) -> @owned A2 {
  // CHECK-NEXT:  bb0(%0 : $A2, %1 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %2 = function_ref @_TFV9witnesses16ConformingStruct7classesfRS0_US_7Classes__FT1xQ__Q_ : $@cc(method) @thin <τ_0_0 where τ_0_0 : Classes> (@owned τ_0_0, @inout ConformingStruct) -> @owned τ_0_0
  // CHECK-NEXT:    %3 = apply %2<A2>(%0, %1) : $@cc(method) @thin <τ_0_0 where τ_0_0 : Classes> (@owned τ_0_0, @inout ConformingStruct) -> @owned τ_0_0
  // CHECK-NEXT:    return %3 : $A2
  // CHECK-NEXT:  }
}
func <~>(x: ConformingStruct, y: ConformingStruct) -> ConformingStruct { return x }
// CHECK-LABEL: sil @_TTWV9witnesses16ConformingStructS_1XFS1_oi3ltgUS1___fMQPS1_FTS2_S2__S2_ : $@cc(witness_method) @thin (@out ConformingStruct, @in ConformingStruct, @in ConformingStruct, @thick ConformingStruct.Type) -> () {
// CHECK-NEXT:  bb0(%0 : $*ConformingStruct, %1 : $*ConformingStruct, %2 : $*ConformingStruct, %3 : $@thick ConformingStruct.Type):
// CHECK-NEXT:    %4 = load %1 : $*ConformingStruct
// CHECK-NEXT:    %5 = load %2 : $*ConformingStruct
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    %6 = function_ref @_TF9witnessesoi3ltgFTVS_16ConformingStructS0__S0_ : $@thin (ConformingStruct, ConformingStruct) -> ConformingStruct
// CHECK-NEXT:    %7 = apply %6(%4, %5) : $@thin (ConformingStruct, ConformingStruct) -> ConformingStruct
// CHECK-NEXT:    store %7 to %0 : $*ConformingStruct
// CHECK-NEXT:    %9 = tuple ()
// CHECK-NEXT:    return %9 : $()
// CHECK-NEXT:  }

class ConformingClass : X {
  func selfTypes(#x: ConformingClass) -> ConformingClass { return x }
  // CHECK-LABEL: sil @_TTWC9witnesses15ConformingClassS_1XFS1_9selfTypesUS1___fRQPS1_FT1xS2__S2_ : $@cc(witness_method) @thin (@out ConformingClass, @in ConformingClass, @inout ConformingClass) -> () {
  // CHECK-NEXT:  bb0(%0 : $*ConformingClass, %1 : $*ConformingClass, %2 : $*ConformingClass):
  // -- load and retain 'self' from inout witness 'self' parameter
  // CHECK-NEXT:    %3 = load %2 : $*ConformingClass
  // CHECK-NEXT:    strong_retain %3 : $ConformingClass
  // CHECK-NEXT:    %5 = load %1 : $*ConformingClass
  // CHECK-NEXT:    %6 = class_method %3 : $ConformingClass, #ConformingClass.selfTypes!1 : ConformingClass -> (x: ConformingClass) -> ConformingClass , $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass
  // CHECK-NEXT:    %7 = apply %6(%5, %3) : $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass
  // CHECK-NEXT:    store %7 to %0 : $*ConformingClass
  // CHECK-NEXT:    %9 = tuple ()
  // CHECK-NEXT:    return %9 : $()
  // CHECK-NEXT:  }
  func loadable(#x: Loadable) -> Loadable { return x }
  func addrOnly(#x: AddrOnly) -> AddrOnly { return x }
  func generic<D>(#x: D) -> D { return x }
  func classes<D2: Classes>(#x: D2) -> D2 { return x }
}
func <~>(x: ConformingClass, y: ConformingClass) -> ConformingClass { return x }

extension ConformingClass : ClassBounded { }
// CHECK-LABEL: sil @_TTWC9witnesses15ConformingClassS_12ClassBoundedFS1_9selfTypesUS1___fQPS1_FT1xS2__S2_ : $@cc(witness_method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass {
// CHECK-NEXT:  bb0(%0 : $ConformingClass, %1 : $ConformingClass):
// CHECK-NEXT:    %2 = class_method %1 : $ConformingClass, #ConformingClass.selfTypes!1 : ConformingClass -> (x: ConformingClass) -> ConformingClass , $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass // user: %3
// CHECK-NEXT:    %3 = apply %2(%0, %1) : $@cc(method) @thin (@owned ConformingClass, @owned ConformingClass) -> @owned ConformingClass
// CHECK-NEXT:    return %3 : $ConformingClass
// CHECK-NEXT:  }

struct ConformingAOStruct : X {
  var makeMeAO : AddrOnly

  mutating
  func selfTypes(#x: ConformingAOStruct) -> ConformingAOStruct { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses18ConformingAOStructS_1XFS1_9selfTypesUS1___fRQPS1_FT1xS2__S2_ : $@cc(witness_method) @thin (@out ConformingAOStruct, @in ConformingAOStruct, @inout ConformingAOStruct) -> () {
  // CHECK-NEXT:  bb0(%0 : $*ConformingAOStruct, %1 : $*ConformingAOStruct, %2 : $*ConformingAOStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses18ConformingAOStruct9selfTypesfRS0_FT1xS0__S0_ : $@cc(method) @thin (@out ConformingAOStruct, @in ConformingAOStruct, @inout ConformingAOStruct) -> ()
  // CHECK-NEXT:    %4 = apply %3(%0, %1, %2) : $@cc(method) @thin (@out ConformingAOStruct, @in ConformingAOStruct, @inout ConformingAOStruct) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }
  func loadable(#x: Loadable) -> Loadable { return x }
  func addrOnly(#x: AddrOnly) -> AddrOnly { return x }
  func generic<D>(#x: D) -> D { return x }
  func classes<D2: Classes>(#x: D2) -> D2 { return x }
}
func <~>(x: ConformingAOStruct, y: ConformingAOStruct) -> ConformingAOStruct { return x }

// TODO: The extra abstraction change in these generic witnesses leads to a
// bunch of redundant temporaries that could be avoided by a more sophisticated
// abstraction difference implementation.

struct ConformsWithMoreGeneric : X, Y {
  mutating
  func selfTypes<E>(#x: E) -> E { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses23ConformsWithMoreGenericS_1XFS1_9selfTypesUS1___fRQPS1_FT1xS2__S2_ : $@cc(witness_method) @thin (@out ConformsWithMoreGeneric, @in ConformsWithMoreGeneric, @inout ConformsWithMoreGeneric) -> () {
  // CHECK-NEXT:  bb0(%0 : $*ConformsWithMoreGeneric, %1 : $*ConformsWithMoreGeneric, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    %3 = load %1 : $*ConformsWithMoreGeneric
  // CHECK-NEXT:    %4 = alloc_stack $ConformsWithMoreGeneric
  // CHECK-NEXT:    store %3 to %4#1 : $*ConformsWithMoreGeneric
  // CHECK-NEXT:    %6 = alloc_stack $ConformsWithMoreGeneric
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %7 = function_ref @_TFV9witnesses23ConformsWithMoreGeneric9selfTypesfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %8 = apply %7<ConformsWithMoreGeneric>(%6#1, %4#1, %2) : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %9 = load %6#1 : $*ConformsWithMoreGeneric
  // CHECK-NEXT:    store %9 to %0 : $*ConformsWithMoreGeneric
  // CHECK-NEXT:    %11 = tuple ()
  // CHECK-NEXT:    dealloc_stack %6#0 : $*@local_storage ConformsWithMoreGeneric
  // CHECK-NEXT:    dealloc_stack %4#0 : $*@local_storage ConformsWithMoreGeneric
  // CHECK-NEXT:    return %11 : $()
  // CHECK-NEXT:  }
  func loadable<F>(#x: F) -> F { return x }
  mutating
  func addrOnly<G>(#x: G) -> G { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses23ConformsWithMoreGenericS_1XFS1_8addrOnlyUS1___fRQPS1_FT1xPS_8AddrOnly__PS3__ : $@cc(witness_method) @thin (@out AddrOnly, @in AddrOnly, @inout ConformsWithMoreGeneric) -> () {
  // CHECK-NEXT:  bb0(%0 : $*AddrOnly, %1 : $*AddrOnly, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses23ConformsWithMoreGeneric8addrOnlyfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %4 = apply %3<AddrOnly>(%0, %1, %2) : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }

  mutating
  func generic<H>(#x: H) -> H { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses23ConformsWithMoreGenericS_1XFS1_7genericUS1___fRQPS1_U__FT1xQ__Q_ : $@cc(witness_method) @thin <A> (@out A, @in A, @inout ConformsWithMoreGeneric) -> () {
  // CHECK-NEXT:  bb0(%0 : $*A, %1 : $*A, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses23ConformsWithMoreGeneric7genericfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %4 = apply %3<A>(%0, %1, %2) : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    return %4 : $()
  // CHECK-NEXT:  }

  mutating
  func classes<I>(#x: I) -> I { return x }
  // CHECK-LABEL: sil @_TTWV9witnesses23ConformsWithMoreGenericS_1XFS1_7classesUS1___fRQPS1_US_7Classes__FT1xQ__Q_ : $@cc(witness_method) @thin <A2 where A2 : Classes> (@owned A2, @inout ConformsWithMoreGeneric) -> @owned A2 {
  // CHECK-NEXT:  bb0(%0 : $A2, %1 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    %2 = alloc_stack $A2
  // CHECK-NEXT:    store %0 to %2#1 : $*A2
  // CHECK-NEXT:    %4 = alloc_stack $A2
  // CHECK-NEXT:    // function_ref witnesses.ConformsWithMoreGeneric.classes (@inout witnesses.ConformsWithMoreGeneric)<A>(x : A) -> A
  // CHECK-NEXT:    %5 = function_ref @_TFV9witnesses23ConformsWithMoreGeneric7classesfRS0_U__FT1xQ__Q_ : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %6 = apply %5<A2>(%4#1, %2#1, %1) : $@cc(method) @thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @inout ConformsWithMoreGeneric) -> ()
  // CHECK-NEXT:    %7 = load %4#1 : $*A2
  // CHECK-NEXT:    dealloc_stack %4#0 : $*@local_storage A2
  // CHECK-NEXT:    dealloc_stack %2#0 : $*@local_storage A2
  // CHECK-NEXT:    return %7 : $A2
  // CHECK-NEXT:  }
}
func <~> <J: Y, K: Y>(x: J, y: K) -> K { return y }
// CHECK-LABEL: sil @_TTWV9witnesses23ConformsWithMoreGenericS_1XFS1_oi3ltgUS1___fMQPS1_FTS2_S2__S2_ : $@cc(witness_method) @thin (@out ConformsWithMoreGeneric, @in ConformsWithMoreGeneric, @in ConformsWithMoreGeneric, @thick ConformsWithMoreGeneric.Type) -> () {
// CHECK-NEXT:  bb0(%0 : $*ConformsWithMoreGeneric, %1 : $*ConformsWithMoreGeneric, %2 : $*ConformsWithMoreGeneric, %3 : $@thick ConformsWithMoreGeneric.Type):
// CHECK-NEXT:    %4 = load %1 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %5 = load %2 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %6 = alloc_stack $ConformsWithMoreGeneric
// CHECK-NEXT:    store %4 to %6#1 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %8 = alloc_stack $ConformsWithMoreGeneric
// CHECK-NEXT:    store %5 to %8#1 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %10 = alloc_stack $ConformsWithMoreGeneric
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    %11 = function_ref @_TF9witnessesoi3ltgUS_1Y_S0___FTQ_Q0__Q0_ : $@thin <τ_0_0, τ_0_1 where τ_0_0 : Y, τ_0_1 : Y> (@out τ_0_1, @in τ_0_0, @in τ_0_1) -> ()
// CHECK-NEXT:    %12 = apply %11<ConformsWithMoreGeneric, ConformsWithMoreGeneric>(%10#1, %6#1, %8#1) : $@thin <τ_0_0, τ_0_1 where τ_0_0 : Y, τ_0_1 : Y> (@out τ_0_1, @in τ_0_0, @in τ_0_1) -> ()
// CHECK-NEXT:    %13 = load %10#1 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    store %13 to %0 : $*ConformsWithMoreGeneric
// CHECK-NEXT:    %15 = tuple ()
// CHECK-NEXT:    dealloc_stack %10#0 : $*@local_storage ConformsWithMoreGeneric
// CHECK-NEXT:    dealloc_stack %8#0 : $*@local_storage ConformsWithMoreGeneric
// CHECK-NEXT:    dealloc_stack %6#0 : $*@local_storage ConformsWithMoreGeneric
// CHECK-NEXT:    return %15 : $()
// CHECK-NEXT:  }

protocol LabeledRequirement {
  func method(#x: Loadable)
}

struct UnlabeledWitness : LabeledRequirement {
  // CHECK-LABEL: sil @_TTWV9witnesses16UnlabeledWitnessS_18LabeledRequirementFS1_6methodUS1___fRQPS1_FT1xVS_8Loadable_T_ : $@cc(witness_method) @thin (Loadable, @inout UnlabeledWitness) -> ()
  func method(x _: Loadable) {}
}

protocol LabeledSelfRequirement {
  func method(#x: Self)
}

struct UnlabeledSelfWitness : LabeledSelfRequirement {
  // CHECK-LABEL: sil @_TTWV9witnesses20UnlabeledSelfWitnessS_22LabeledSelfRequirementFS1_6methodUS1___fRQPS1_FT1xS2__T_ : $@cc(witness_method) @thin (@in UnlabeledSelfWitness, @inout UnlabeledSelfWitness) -> ()
  func method(x _: UnlabeledSelfWitness) {}
}

protocol UnlabeledRequirement {
  func method(x _: Loadable)
}

struct LabeledWitness : UnlabeledRequirement {
  // CHECK-LABEL: sil @_TTWV9witnesses14LabeledWitnessS_20UnlabeledRequirementFS1_6methodUS1___fRQPS1_FT1xVS_8Loadable_T_ : $@cc(witness_method) @thin (Loadable, @inout LabeledWitness) -> ()
  func method(#x: Loadable) {}
}

protocol UnlabeledSelfRequirement {
  func method(_: Self)
}

struct LabeledSelfWitness : UnlabeledSelfRequirement {
  // CHECK-LABEL: sil @_TTWV9witnesses18LabeledSelfWitnessS_24UnlabeledSelfRequirementFS1_6methodUS1___fRQPS1_FS2_T_ : $@cc(witness_method) @thin (@in LabeledSelfWitness, @inout LabeledSelfWitness) -> ()
  func method(x: LabeledSelfWitness) {}
}
