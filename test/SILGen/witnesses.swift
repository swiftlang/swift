
// RUN: %target-swift-emit-silgen -module-name witnesses -Xllvm -sil-full-demangle %s -disable-objc-attr-requires-foundation-module -enable-objc-interop -enable-sil-ownership | %FileCheck %s

infix operator <~>

func archetype_method<T: X>(x: T, y: T) -> T {
  var x = x
  var y = y
  return x.selfTypes(x: y)
}
// CHECK-LABEL: sil hidden @$s9witnesses16archetype_method{{[_0-9a-zA-Z]*}}F{{.*}} : $@convention(thin) <T where T : X> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// CHECK:         [[METHOD:%.*]] = witness_method $T, #X.selfTypes!1 : {{.*}} : $@convention(witness_method: X) <τ_0_0 where τ_0_0 : X> (@in_guaranteed τ_0_0, @inout τ_0_0) -> @out τ_0_0
// CHECK:         apply [[METHOD]]<T>({{%.*}}, {{%.*}}, {{%.*}}) : $@convention(witness_method: X) <τ_0_0 where τ_0_0 : X> (@in_guaranteed τ_0_0, @inout τ_0_0) -> @out τ_0_0
// CHECK:       }

func archetype_generic_method<T: X>(x: T, y: Loadable) -> Loadable {
  var x = x
  return x.generic(x: y)
}
// CHECK-LABEL: sil hidden @$s9witnesses24archetype_generic_method{{[_0-9a-zA-Z]*}}F{{.*}} : $@convention(thin) <T where T : X> (@in_guaranteed T, Loadable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = witness_method $T, #X.generic!1 : {{.*}} : $@convention(witness_method: X) <τ_0_0 where τ_0_0 : X><τ_1_0> (@in_guaranteed τ_1_0, @inout τ_0_0) -> @out τ_1_0
// CHECK:         apply [[METHOD]]<T, Loadable>({{%.*}}, {{%.*}}, {{%.*}}) : $@convention(witness_method: X) <τ_0_0 where τ_0_0 : X><τ_1_0> (@in_guaranteed τ_1_0, @inout τ_0_0) -> @out τ_1_0
// CHECK:       }

// CHECK-LABEL: sil hidden @$s9witnesses32archetype_associated_type_method{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T where T : WithAssoc> (@in_guaranteed T, @in_guaranteed T.AssocType) -> @out T
// CHECK:         apply %{{[0-9]+}}<T>
func archetype_associated_type_method<T: WithAssoc>(x: T, y: T.AssocType) -> T {
  return x.useAssocType(x: y)
}

protocol StaticMethod { static func staticMethod() }

// CHECK-LABEL: sil hidden @$s9witnesses23archetype_static_method{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T where T : StaticMethod> (@in_guaranteed T) -> ()
func archetype_static_method<T: StaticMethod>(x: T) {
  // CHECK: [[METHOD:%.*]] = witness_method $T, #StaticMethod.staticMethod!1 : {{.*}} : $@convention(witness_method: StaticMethod) <τ_0_0 where τ_0_0 : StaticMethod> (@thick τ_0_0.Type) -> ()
  // CHECK: apply [[METHOD]]<T>
  T.staticMethod()
}

protocol Existentiable {
  func foo() -> Loadable
  func generic<T>() -> T
}

func protocol_method(x: Existentiable) -> Loadable {
  return x.foo()
}
// CHECK-LABEL: sil hidden @$s9witnesses15protocol_method1xAA8LoadableVAA13Existentiable_p_tF : $@convention(thin) (@in_guaranteed Existentiable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = witness_method $[[OPENED:@opened(.*) Existentiable]], #Existentiable.foo!1
// CHECK:         apply [[METHOD]]<[[OPENED]]>({{%.*}})
// CHECK:       }

func protocol_generic_method(x: Existentiable) -> Loadable {
  return x.generic()
}
// CHECK-LABEL: sil hidden @$s9witnesses23protocol_generic_method1xAA8LoadableVAA13Existentiable_p_tF : $@convention(thin) (@in_guaranteed Existentiable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = witness_method $[[OPENED:@opened(.*) Existentiable]], #Existentiable.generic!1
// CHECK:         apply [[METHOD]]<[[OPENED]], Loadable>({{%.*}}, {{%.*}})
// CHECK:       }

@objc protocol ObjCAble {
  func foo()
}

// CHECK-LABEL: sil hidden @$s9witnesses20protocol_objc_method1xyAA8ObjCAble_p_tF : $@convention(thin) (@guaranteed ObjCAble) -> ()
// CHECK:         objc_method {{%.*}} : $@opened({{.*}}) ObjCAble, #ObjCAble.foo!1.foreign
func protocol_objc_method(x: ObjCAble) {
  x.foo()
}

struct Loadable {}
protocol AddrOnly {}
protocol Classes : class {}

protocol X {
  mutating
  func selfTypes(x: Self) -> Self
  mutating
  func loadable(x: Loadable) -> Loadable
  mutating
  func addrOnly(x: AddrOnly) -> AddrOnly
  mutating
  func generic<A>(x: A) -> A
  mutating
  func classes<A2: Classes>(x: A2) -> A2
  static func <~>(_ x: Self, y: Self) -> Self
}
protocol Y {}

protocol WithAssoc {
  associatedtype AssocType
  func useAssocType(x: AssocType) -> Self
}

protocol ClassBounded : class {
  func selfTypes(x: Self) -> Self
}

struct ConformingStruct : X {
  mutating
  func selfTypes(x: ConformingStruct) -> ConformingStruct { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16ConformingStructVAA1XA2aDP9selfTypes{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) (@in_guaranteed ConformingStruct, @inout ConformingStruct) -> @out ConformingStruct {
  // CHECK:       bb0(%0 : @trivial $*ConformingStruct, %1 : @trivial $*ConformingStruct, %2 : @trivial $*ConformingStruct):
  // CHECK-NEXT:    %3 = load [trivial] %1 : $*ConformingStruct
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %4 = function_ref @$s9witnesses16ConformingStructV9selfTypes{{[_0-9a-zA-Z]*}}F : $@convention(method) (ConformingStruct, @inout ConformingStruct) -> ConformingStruct
  // CHECK-NEXT:    %5 = apply %4(%3, %2) : $@convention(method) (ConformingStruct, @inout ConformingStruct) -> ConformingStruct
  // CHECK-NEXT:    store %5 to [trivial] %0 : $*ConformingStruct
  // CHECK-NEXT:    %7 = tuple ()
  // CHECK-NEXT:    return %7 : $()
  // CHECK-NEXT:  }
  
  mutating
  func loadable(x: Loadable) -> Loadable { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16ConformingStructVAA1XA2aDP8loadable{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) (Loadable, @inout ConformingStruct) -> Loadable {
  // CHECK:       bb0(%0 : @trivial $Loadable, %1 : @trivial $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %2 = function_ref @$s9witnesses16ConformingStructV8loadable{{[_0-9a-zA-Z]*}}F : $@convention(method) (Loadable, @inout ConformingStruct) -> Loadable
  // CHECK-NEXT:    %3 = apply %2(%0, %1) : $@convention(method) (Loadable, @inout ConformingStruct) -> Loadable
  // CHECK-NEXT:    return %3 : $Loadable
  // CHECK-NEXT:  }
  
  mutating
  func addrOnly(x: AddrOnly) -> AddrOnly { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16ConformingStructVAA1XA2aDP8addrOnly{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) (@in_guaranteed AddrOnly, @inout ConformingStruct) -> @out AddrOnly {
  // CHECK:       bb0(%0 : @trivial $*AddrOnly, %1 : @trivial $*AddrOnly, %2 : @trivial $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @$s9witnesses16ConformingStructV8addrOnly{{[_0-9a-zA-Z]*}}F : $@convention(method) (@in_guaranteed AddrOnly, @inout ConformingStruct) -> @out AddrOnly
  // CHECK-NEXT:    %4 = apply %3(%0, %1, %2) : $@convention(method) (@in_guaranteed AddrOnly, @inout ConformingStruct) -> @out AddrOnly
  // CHECK-NEXT:    %5 = tuple ()
  // CHECK-NEXT:    return %5 : $()
  // CHECK-NEXT:  }
  
  mutating
  func generic<C>(x: C) -> C { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16ConformingStructVAA1XA2aDP7generic{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformingStruct) -> @out τ_0_0 {
  // CHECK:       bb0(%0 : @trivial $*τ_0_0, %1 : @trivial $*τ_0_0, %2 : @trivial $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @$s9witnesses16ConformingStructV7generic{{[_0-9a-zA-Z]*}}F : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformingStruct) -> @out τ_0_0
  // CHECK-NEXT:    %4 = apply %3<τ_0_0>(%0, %1, %2) : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformingStruct) -> @out τ_0_0
  // CHECK-NEXT:    %5 = tuple ()
  // CHECK-NEXT:    return %5 : $()
  // CHECK-NEXT:  }
  mutating
  func classes<C2: Classes>(x: C2) -> C2 { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16ConformingStructVAA1XA2aDP7classes{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) <τ_0_0 where τ_0_0 : Classes> (@guaranteed τ_0_0, @inout ConformingStruct) -> @owned τ_0_0 {
  // CHECK:       bb0(%0 : @guaranteed $τ_0_0, %1 : @trivial $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %2 = function_ref @$s9witnesses16ConformingStructV7classes{{[_0-9a-zA-Z]*}}F : $@convention(method) <τ_0_0 where τ_0_0 : Classes> (@guaranteed τ_0_0, @inout ConformingStruct) -> @owned τ_0_0
  // CHECK-NEXT:    %3 = apply %2<τ_0_0>(%0, %1) : $@convention(method) <τ_0_0 where τ_0_0 : Classes> (@guaranteed τ_0_0, @inout ConformingStruct) -> @owned τ_0_0
  // CHECK-NEXT:    return %3 : $τ_0_0
  // CHECK-NEXT:  }
}
func <~>(_ x: ConformingStruct, y: ConformingStruct) -> ConformingStruct { return x }
// CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16ConformingStructVAA1XA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW : $@convention(witness_method: X) (@in_guaranteed ConformingStruct, @in_guaranteed ConformingStruct, @thick ConformingStruct.Type) -> @out ConformingStruct {
// CHECK:       bb0([[ARG1:%.*]] : @trivial $*ConformingStruct, [[ARG2:%.*]] : @trivial $*ConformingStruct, [[ARG3:%.*]] : @trivial $*ConformingStruct, [[ARG4:%.*]] : @trivial $@thick ConformingStruct.Type):
// CHECK-NEXT:    [[LOADED_ARG2:%.*]] = load [trivial] [[ARG2]] : $*ConformingStruct
// CHECK-NEXT:    [[LOADED_ARG3:%.*]] = load [trivial] [[ARG3]] : $*ConformingStruct
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FUNC:%.*]] = function_ref @$s9witnesses3ltgoiyAA16ConformingStructVAD_ADtF : $@convention(thin) (ConformingStruct, ConformingStruct) -> ConformingStruct
// CHECK-NEXT:    [[FUNC_RESULT:%.*]] = apply [[FUNC]]([[LOADED_ARG2]], [[LOADED_ARG3]]) : $@convention(thin) (ConformingStruct, ConformingStruct) -> ConformingStruct
// CHECK-NEXT:    store [[FUNC_RESULT]] to [trivial] [[ARG1]] : $*ConformingStruct
// CHECK-NEXT:    %9 = tuple ()
// CHECK-NEXT:    return %9 : $()
// CHECK-NEXT:  }

final class ConformingClass : X {
  func selfTypes(x: ConformingClass) -> ConformingClass { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses15ConformingClassCAA1XA2aDP9selfTypes{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) (@in_guaranteed ConformingClass, @inout ConformingClass) -> @out ConformingClass {
  // CHECK:  bb0([[ARG1:%.*]] : @trivial $*ConformingClass, [[ARG2:%.*]] : @trivial $*ConformingClass, [[ARG3:%.*]] : @trivial $*ConformingClass):
  // -- load and copy_value 'self' from inout witness 'self' parameter
  // CHECK:    [[ARG2_LOADED:%.*]] = load_borrow [[ARG2]] : $*ConformingClass
  // CHECK:    [[ARG3_LOADED:%.*]] = load_borrow [[ARG3]] : $*ConformingClass
  // CHECK:    [[FUNC:%.*]] = function_ref @$s9witnesses15ConformingClassC9selfTypes{{[_0-9a-zA-Z]*}}F
  // CHECK:    [[FUNC_RESULT:%.*]] = apply [[FUNC]]([[ARG2_LOADED]], [[ARG3_LOADED]]) : $@convention(method) (@guaranteed ConformingClass, @guaranteed ConformingClass) -> @owned ConformingClass
  // CHECK:    store [[FUNC_RESULT]] to [init] [[ARG1]] : $*ConformingClass
  // CHECK:    end_borrow [[ARG3_LOADED]]
  // CHECK:  } // end sil function '$s9witnesses15ConformingClassCAA1XA2aDP9selfTypes{{[_0-9a-zA-Z]*}}FTW'
  func loadable(x: Loadable) -> Loadable { return x }
  func addrOnly(x: AddrOnly) -> AddrOnly { return x }
  func generic<D>(x: D) -> D { return x }
  func classes<D2: Classes>(x: D2) -> D2 { return x }
}
func <~>(_ x: ConformingClass, y: ConformingClass) -> ConformingClass { return x }

extension ConformingClass : ClassBounded { }
// CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses15ConformingClassCAA0C7BoundedA2aDP9selfTypes{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: ClassBounded) (@guaranteed ConformingClass, @guaranteed ConformingClass) -> @owned ConformingClass {
// CHECK:  bb0([[C0:%.*]] : @guaranteed $ConformingClass, [[C1:%.*]] : @guaranteed $ConformingClass):
// CHECK-NEXT:    function_ref
// CHECK-NEXT:    [[FUN:%.*]] = function_ref @$s9witnesses15ConformingClassC9selfTypes{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[FUN]]([[C0]], [[C1]]) : $@convention(method) (@guaranteed ConformingClass, @guaranteed ConformingClass) -> @owned ConformingClass
// CHECK-NEXT:    return [[RESULT]] : $ConformingClass
// CHECK-NEXT:  }

struct ConformingAOStruct : X {
  var makeMeAO : AddrOnly

  mutating
  func selfTypes(x: ConformingAOStruct) -> ConformingAOStruct { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses18ConformingAOStructVAA1XA2aDP9selfTypes{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) (@in_guaranteed ConformingAOStruct, @inout ConformingAOStruct) -> @out ConformingAOStruct {
  // CHECK:       bb0(%0 : @trivial $*ConformingAOStruct, %1 : @trivial $*ConformingAOStruct, %2 : @trivial $*ConformingAOStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @$s9witnesses18ConformingAOStructV9selfTypes{{[_0-9a-zA-Z]*}}F : $@convention(method) (@in_guaranteed ConformingAOStruct, @inout ConformingAOStruct) -> @out ConformingAOStruct
  // CHECK-NEXT:    %4 = apply %3(%0, %1, %2) : $@convention(method) (@in_guaranteed ConformingAOStruct, @inout ConformingAOStruct) -> @out ConformingAOStruct
  // CHECK-NEXT:    %5 = tuple ()
  // CHECK-NEXT:    return %5 : $()
  // CHECK-NEXT:  }
  func loadable(x: Loadable) -> Loadable { return x }
  func addrOnly(x: AddrOnly) -> AddrOnly { return x }
  func generic<D>(x: D) -> D { return x }
  func classes<D2: Classes>(x: D2) -> D2 { return x }
}
func <~>(_ x: ConformingAOStruct, y: ConformingAOStruct) -> ConformingAOStruct { return x }

struct ConformsWithMoreGeneric : X, Y {
  mutating
  func selfTypes<E>(x: E) -> E { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses23ConformsWithMoreGenericVAA1XA2aDP9selfTypes{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) (@in_guaranteed ConformsWithMoreGeneric, @inout ConformsWithMoreGeneric) -> @out ConformsWithMoreGeneric {
  // CHECK:       bb0(%0 : @trivial $*ConformsWithMoreGeneric, %1 : @trivial $*ConformsWithMoreGeneric, %2 : @trivial $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    [[WITNESS_FN:%.*]] = function_ref @$s9witnesses23ConformsWithMoreGenericV9selfTypes{{[_0-9a-zA-Z]*}}F : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = apply [[WITNESS_FN]]<ConformsWithMoreGeneric>(%0, %1, %2) : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RESULT]] : $()
  // CHECK-NEXT:  }
  func loadable<F>(x: F) -> F { return x }
  mutating
  func addrOnly<G>(x: G) -> G { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses23ConformsWithMoreGenericVAA1XA2aDP8addrOnly{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) (@in_guaranteed AddrOnly, @inout ConformsWithMoreGeneric) -> @out AddrOnly {
  // CHECK:       bb0(%0 : @trivial $*AddrOnly, %1 : @trivial $*AddrOnly, %2 : @trivial $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @$s9witnesses23ConformsWithMoreGenericV8addrOnly{{[_0-9a-zA-Z]*}}F : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    %4 = apply %3<AddrOnly>(%0, %1, %2) : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RESULT]] : $()
  // CHECK-NEXT:  }

  mutating
  func generic<H>(x: H) -> H { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses23ConformsWithMoreGenericVAA1XA2aDP7generic{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0 {
  // CHECK:       bb0(%0 : @trivial $*τ_0_0, %1 : @trivial $*τ_0_0, %2 : @trivial $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @$s9witnesses23ConformsWithMoreGenericV7generic{{[_0-9a-zA-Z]*}}F : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    %4 = apply %3<τ_0_0>(%0, %1, %2) : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RESULT]] : $()
  // CHECK-NEXT:  }

  mutating
  func classes<I>(x: I) -> I { return x }
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses23ConformsWithMoreGenericVAA1XA2aDP7classes{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: X) <τ_0_0 where τ_0_0 : Classes> (@guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @owned τ_0_0 {
  // CHECK:       bb0([[ARG0:%.*]] : @guaranteed $τ_0_0, [[ARG1:%.*]] : @trivial $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    [[SELF_BOX:%.*]] = alloc_stack $τ_0_0
  // CHECK-NEXT:    [[ARG0_COPY:%.*]] = copy_value [[ARG0]]
  // CHECK-NEXT:    store [[ARG0_COPY]] to [init] [[SELF_BOX]] : $*τ_0_0
  // CHECK-NEXT:    // function_ref witnesses.ConformsWithMoreGeneric.classes
  // CHECK-NEXT:    [[WITNESS_FN:%.*]] = function_ref @$s9witnesses23ConformsWithMoreGenericV7classes{{[_0-9a-zA-Z]*}}F : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT_BOX:%.*]] = alloc_stack $τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = apply [[WITNESS_FN]]<τ_0_0>([[RESULT_BOX]], [[SELF_BOX]], %1) : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = load [take] [[RESULT_BOX]] : $*τ_0_0
  // CHECK-NEXT:    dealloc_stack [[RESULT_BOX]] : $*τ_0_0
  // CHECK-NEXT:    destroy_addr [[SELF_BOX]]
  // CHECK-NEXT:    dealloc_stack [[SELF_BOX]] : $*τ_0_0
  // CHECK-NEXT:    return [[RESULT]] : $τ_0_0
  // CHECK-NEXT:  }
}
func <~> <J: Y, K: Y>(_ x: J, y: K) -> K { return y }
// CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses23ConformsWithMoreGenericVAA1XA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW : $@convention(witness_method: X) (@in_guaranteed ConformsWithMoreGeneric, @in_guaranteed ConformsWithMoreGeneric, @thick ConformsWithMoreGeneric.Type) -> @out ConformsWithMoreGeneric {
// CHECK:       bb0(%0 : @trivial $*ConformsWithMoreGeneric, %1 : @trivial $*ConformsWithMoreGeneric, %2 : @trivial $*ConformsWithMoreGeneric, %3 : @trivial $@thick ConformsWithMoreGeneric.Type):
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[WITNESS_FN:%.*]] = function_ref @$s9witnesses3ltgoi{{[_0-9a-zA-Z]*}}F : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Y, τ_0_1 : Y> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @out τ_0_1
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[WITNESS_FN]]<ConformsWithMoreGeneric, ConformsWithMoreGeneric>(%0, %1, %2) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Y, τ_0_1 : Y> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @out τ_0_1
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:    return [[RESULT]] : $()
// CHECK-NEXT:  }

protocol LabeledRequirement {
  func method(x: Loadable)
}

struct UnlabeledWitness : LabeledRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16UnlabeledWitnessVAA18LabeledRequirementA2aDP6method{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: LabeledRequirement) (Loadable, @in_guaranteed UnlabeledWitness) -> ()
  func method(x _: Loadable) {}
}

protocol LabeledSelfRequirement {
  func method(x: Self)
}

struct UnlabeledSelfWitness : LabeledSelfRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses20UnlabeledSelfWitnessVAA07LabeledC11RequirementA2aDP6method{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: LabeledSelfRequirement) (@in_guaranteed UnlabeledSelfWitness, @in_guaranteed UnlabeledSelfWitness) -> ()
  func method(x _: UnlabeledSelfWitness) {}
}

protocol UnlabeledRequirement {
  func method(x _: Loadable)
}

struct LabeledWitness : UnlabeledRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses14LabeledWitnessVAA20UnlabeledRequirementA2aDP6method{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: UnlabeledRequirement) (Loadable, @in_guaranteed LabeledWitness) -> ()
  func method(x: Loadable) {}
}

protocol UnlabeledSelfRequirement {
  func method(_: Self)
}

struct LabeledSelfWitness : UnlabeledSelfRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses18LabeledSelfWitnessVAA09UnlabeledC11RequirementA2aDP6method{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: UnlabeledSelfRequirement) (@in_guaranteed LabeledSelfWitness, @in_guaranteed LabeledSelfWitness) -> ()
  func method(_ x: LabeledSelfWitness) {}
}

protocol ReadOnlyRequirement {
  var prop: String { get }
  static var prop: String { get }
}

struct ImmutableModel: ReadOnlyRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses14ImmutableModelVAA19ReadOnlyRequirementA2aDP4propSSvgTW : $@convention(witness_method: ReadOnlyRequirement) (@in_guaranteed ImmutableModel) -> @owned String
  let prop: String = "a"
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses14ImmutableModelVAA19ReadOnlyRequirementA2aDP4propSSvgZTW : $@convention(witness_method: ReadOnlyRequirement) (@thick ImmutableModel.Type) -> @owned String
  static let prop: String = "b"
}

protocol FailableRequirement {
  init?(foo: Int)
}

protocol NonFailableRefinement: FailableRequirement {
  init(foo: Int)
}

protocol IUOFailableRequirement {
  init!(foo: Int)
}

struct NonFailableModel: FailableRequirement, NonFailableRefinement, IUOFailableRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16NonFailableModelVAA0C11Requirement{{[_0-9a-zA-Z]*}}fCTW : $@convention(witness_method: FailableRequirement) (Int, @thick NonFailableModel.Type) -> @out Optional<NonFailableModel>
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16NonFailableModelVAA0bC10Refinement{{[_0-9a-zA-Z]*}}fCTW : $@convention(witness_method: NonFailableRefinement) (Int, @thick NonFailableModel.Type) -> @out NonFailableModel
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16NonFailableModelVAA22IUOFailableRequirement{{[_0-9a-zA-Z]*}}fCTW : $@convention(witness_method: IUOFailableRequirement) (Int, @thick NonFailableModel.Type) -> @out Optional<NonFailableModel>
  init(foo: Int) {}
}

struct FailableModel: FailableRequirement, IUOFailableRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses13FailableModelVAA0B11Requirement{{[_0-9a-zA-Z]*}}fCTW

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses13FailableModelVAA22IUOFailableRequirement{{[_0-9a-zA-Z]*}}fCTW
  // CHECK: bb0([[SELF:%[0-9]+]] : @trivial $*Optional<FailableModel>, [[FOO:%[0-9]+]] : @trivial $Int, [[META:%[0-9]+]] : @trivial $@thick FailableModel.Type):
  // CHECK: [[FN:%.*]] = function_ref @$s9witnesses13FailableModelV{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[INNER:%.*]] = apply [[FN]](
  // CHECK: store [[INNER]] to [trivial] [[SELF]]
  // CHECK: return
  init?(foo: Int) {}
}

struct IUOFailableModel : NonFailableRefinement, IUOFailableRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16IUOFailableModelVAA21NonFailableRefinement{{[_0-9a-zA-Z]*}}fCTW
  // CHECK: bb0([[SELF:%[0-9]+]] : @trivial $*IUOFailableModel, [[FOO:%[0-9]+]] : @trivial $Int, [[META:%[0-9]+]] : @trivial $@thick IUOFailableModel.Type):
  // CHECK:   [[META:%[0-9]+]] = metatype $@thin IUOFailableModel.Type
  // CHECK:   [[INIT:%[0-9]+]] = function_ref @$s9witnesses16IUOFailableModelV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Int, @thin IUOFailableModel.Type) -> Optional<IUOFailableModel>
  // CHECK:   [[IUO_RESULT:%[0-9]+]] = apply [[INIT]]([[FOO]], [[META]]) : $@convention(method) (Int, @thin IUOFailableModel.Type) -> Optional<IUOFailableModel>
  // CHECK: bb2([[RESULT:%.*]] : @trivial $IUOFailableModel):
  // CHECK:   store [[RESULT]] to [trivial] [[SELF]] : $*IUOFailableModel
  // CHECK:   return
  init!(foo: Int) { return nil }
}

protocol FailableClassRequirement: class {
  init?(foo: Int)
}

protocol NonFailableClassRefinement: FailableClassRequirement {
  init(foo: Int)
}

protocol IUOFailableClassRequirement: class {
  init!(foo: Int)
}

final class NonFailableClassModel: FailableClassRequirement, NonFailableClassRefinement, IUOFailableClassRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses21NonFailableClassModelCAA0cD11Requirement{{[_0-9a-zA-Z]*}}fCTW : $@convention(witness_method: FailableClassRequirement) (Int, @thick NonFailableClassModel.Type) -> @owned Optional<NonFailableClassModel>
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses21NonFailableClassModelCAA0bcD10Refinement{{[_0-9a-zA-Z]*}}fCTW : $@convention(witness_method: NonFailableClassRefinement) (Int, @thick NonFailableClassModel.Type) -> @owned NonFailableClassModel
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses21NonFailableClassModelCAA011IUOFailableD11Requirement{{[_0-9a-zA-Z]*}}fCTW : $@convention(witness_method: IUOFailableClassRequirement) (Int, @thick NonFailableClassModel.Type) -> @owned Optional<NonFailableClassModel>
  init(foo: Int) {}
}

final class FailableClassModel: FailableClassRequirement, IUOFailableClassRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses18FailableClassModelCAA0bC11Requirement{{[_0-9a-zA-Z]*}}fCTW

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses18FailableClassModelCAA011IUOFailableC11Requirement{{[_0-9a-zA-Z]*}}fCTW
  // CHECK: [[FUNC:%.*]] = function_ref @$s9witnesses18FailableClassModelC{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[INNER:%.*]] = apply [[FUNC]](%0, %1)
  // CHECK: return [[INNER]] : $Optional<FailableClassModel>
  init?(foo: Int) {}
}

final class IUOFailableClassModel: NonFailableClassRefinement, IUOFailableClassRequirement {
  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses21IUOFailableClassModelCAA011NonFailableC10Refinement{{[_0-9a-zA-Z]*}}fCTW
  // CHECK: bb0({{.*}}):
  // CHECK:   [[FUNC:%.*]] = function_ref @$s9witnesses21IUOFailableClassModelC3fooACSgSi_tcfC : $@convention(method) (Int, @thick IUOFailableClassModel.Type) -> @owned Optional<IUOFailableClassModel>
  // CHECK:   [[VALUE:%.*]] = apply [[FUNC]]({{.*}})
  // CHECK:   switch_enum [[VALUE]] : $Optional<IUOFailableClassModel>, case #Optional.some!enumelt.1: [[SOMEBB:bb[0-9]+]], case #Optional.none!enumelt: [[NONEBB:bb[0-9]+]]
  //
  // CHECK: [[NONEBB]]:
  // CHECK:   unreachable
  //
  // CHECK: [[SOMEBB]]([[RESULT:%.*]] : @owned $IUOFailableClassModel)
  // CHECK: return [[RESULT]] : $IUOFailableClassModel
  // CHECK: } // end sil function '$s9witnesses21IUOFailableClassModelCAA011NonFailableC10Refinement{{[_0-9a-zA-Z]*}}fCTW'

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses21IUOFailableClassModelCAA0bC11Requirement{{[_0-9a-zA-Z]*}}fCTW
  init!(foo: Int) {}

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses21IUOFailableClassModelCAA08FailableC11Requirement{{[_0-9a-zA-Z]*}}fCTW
  // CHECK: [[FUNC:%.*]] = function_ref @$s9witnesses21IUOFailableClassModelC{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[INNER:%.*]] = apply [[FUNC]](%0, %1)
  // CHECK: return [[INNER]] : $Optional<IUOFailableClassModel>
}

protocol HasAssoc {
  associatedtype Assoc
}

protocol GenericParameterNameCollisionProtocol {
  func foo<T>(_ x: T)
  associatedtype Assoc2
  func bar<T>(_ x: (T) -> Assoc2)
}

struct GenericParameterNameCollision<T: HasAssoc> :
    GenericParameterNameCollisionProtocol {

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses29GenericParameterNameCollisionVyxGAA0bcdE8ProtocolA2aEP3fooyyqd__lFTW : $@convention(witness_method: GenericParameterNameCollisionProtocol) <τ_0_0 where τ_0_0 : HasAssoc><τ_1_0> (@in_guaranteed τ_1_0, @in_guaranteed GenericParameterNameCollision<τ_0_0>) -> () {
  // CHECK:       bb0(%0 : @trivial $*τ_1_0, %1 : @trivial $*GenericParameterNameCollision<τ_0_0>):
  // CHECK:         apply {{%.*}}<τ_0_0, τ_1_0>
  func foo<U>(_ x: U) {}

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses29GenericParameterNameCollisionVyxGAA0bcdE8ProtocolA2aEP3baryy6Assoc2Qzqd__XElFTW : $@convention(witness_method: GenericParameterNameCollisionProtocol) <τ_0_0 where τ_0_0 : HasAssoc><τ_1_0> (@noescape @callee_guaranteed (@in_guaranteed τ_1_0) -> @out τ_0_0.Assoc, @in_guaranteed GenericParameterNameCollision<τ_0_0>) -> () {
  // CHECK:       bb0(%0 : @trivial $@noescape @callee_guaranteed (@in_guaranteed τ_1_0) -> @out τ_0_0.Assoc, %1 : @trivial $*GenericParameterNameCollision<τ_0_0>):
  // CHECK:         apply {{%.*}}<τ_0_0, τ_1_0>
  func bar<V>(_ x: (V) -> T.Assoc) {}
}

protocol PropertyRequirement {
  var width: Int { get set }
  static var height: Int { get set }
  var depth: Int { get set }
}

class PropertyRequirementBase {
  var width: Int = 12
  static var height: Int = 13
}

class PropertyRequirementWitnessFromBase : PropertyRequirementBase, PropertyRequirement {
  var depth: Int = 14

  // Make sure the contravariant return type in modify works correctly

  // If the witness is in a base class of the conforming class, make sure we have a bit_cast in there:

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses34PropertyRequirementWitnessFromBaseCAA0bC0A2aDP5widthSivMTW : {{.*}} {
  // CHECK: bb0([[ARG2:%.*]] : @trivial $*PropertyRequirementWitnessFromBase):
  // CHECK-NEXT: [[ARG2_LOADED:%[0-9][0-9]*]] = load_borrow [[ARG2]]
  // CHECK-NEXT: [[CAST_ARG2_LOADED:%[0-9][0-9]*]] = upcast [[ARG2_LOADED]] : $PropertyRequirementWitnessFromBase to $PropertyRequirementBase
  // CHECK-NEXT: [[METH:%.*]] = class_method [[CAST_ARG2_LOADED]] : $PropertyRequirementBase, #PropertyRequirementBase.width!modify.1
  // CHECK-NEXT: ([[RES:%.*]], [[TOKEN:%.*]]) = begin_apply [[METH]]([[CAST_ARG2_LOADED]]) : $@yield_once @convention(method) (@guaranteed PropertyRequirementBase) -> @yields @inout Int
  // CHECK-NEXT: yield [[RES]]
  // CHECK:      end_apply [[TOKEN]]
  // CHECK-NEXT: [[TUPLE:%.*]] = tuple ()
  // CHECK-NEXT: end_borrow [[ARG2_LOADED]]
  // CHECK-NEXT: return [[TUPLE]]

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses34PropertyRequirementWitnessFromBaseCAA0bC0A2aDP6heightSivMZTW : {{.*}} {
  // CHECK: [[OBJ:%.*]] = upcast %0 : $@thick PropertyRequirementWitnessFromBase.Type to $@thick PropertyRequirementBase.Type
  // CHECK: [[METH:%.*]] = function_ref @$s9witnesses23PropertyRequirementBaseC6heightSivMZ
  // CHECK-NEXT: ([[RES:%.*]], [[TOKEN:%.*]]) = begin_apply [[METH]]
  // CHECK-NEXT: yield [[RES]]
  // CHECK:      end_apply [[TOKEN]]
  // CHECK-NEXT: [[TUPLE:%.*]] = tuple ()
  // CHECK-NEXT: return [[TUPLE]]

  // CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses34PropertyRequirementWitnessFromBaseCAA0bC0A2aDP5depthSivMTW
  // CHECK: bb0([[ARG2:%.*]] : @trivial $*PropertyRequirementWitnessFromBase):
  // CHECK: [[ARG2_LOADED:%[0-9][0-9]*]] = load_borrow [[ARG2]]
  // CHECK: [[METH:%.*]] = class_method [[ARG2_LOADED]] : $PropertyRequirementWitnessFromBase, #PropertyRequirementWitnessFromBase.depth!modify.1
  // CHECK-NEXT: ([[RES:%.*]], [[TOKEN:%.*]]) = begin_apply [[METH]]([[ARG2_LOADED]]) : $@yield_once @convention(method) (@guaranteed PropertyRequirementWitnessFromBase) -> @yields @inout Int
  // CHECK-NEXT: yield [[RES]]
  // CHECK:      end_apply [[TOKEN]]
  // CHECK-NEXT: [[TUPLE:%.*]] = tuple ()
  // CHECK-NEXT: end_borrow [[ARG2_LOADED]]
  // CHECK-NEXT: return [[TUPLE]]
}

protocol Crashable {
  func crash()
}

class CrashableBase {
  func crash() {}
}

// CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses16GenericCrashableCyxGAA0C0A2aEP5crashyyFTW : $@convention(witness_method: Crashable) <τ_0_0> (@in_guaranteed GenericCrashable<τ_0_0>) -> ()
// CHECK:       bb0(%0 : @trivial $*GenericCrashable<τ_0_0>):
// CHECK-NEXT: [[SELF:%.*]] = load_borrow %0 : $*GenericCrashable<τ_0_0>
// CHECK-NEXT: [[BASE:%.*]] = upcast [[SELF]] : $GenericCrashable<τ_0_0> to $CrashableBase
// CHECK-NEXT: [[FN:%.*]] = class_method [[BASE]] : $CrashableBase, #CrashableBase.crash!1 : (CrashableBase) -> () -> (), $@convention(method) (@guaranteed CrashableBase) -> ()
// CHECK-NEXT: apply [[FN]]([[BASE]]) : $@convention(method) (@guaranteed CrashableBase) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: end_borrow [[SELF]]
// CHECK-NEXT: return [[RESULT]] : $()

class GenericCrashable<T> : CrashableBase, Crashable {}

// rdar://problem/35297911: allow witness with a noescape parameter to
// match a requirement with an escaping paameter.
protocol EscapingReq {
  func f(_: @escaping (Int) -> Int)
}

// CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses18EscapingCovarianceVAA0B3ReqA2aDP1fyyS2icFTW : $@convention(witness_method: EscapingReq) (@guaranteed @callee_guaranteed (Int) -> Int, @in_guaranteed EscapingCovariance) -> ()
// CHECK-NOT: return
// CHECK: convert_escape_to_noescape [not_guaranteed] %0
// CHECK: return
struct EscapingCovariance: EscapingReq {
  func f(_: (Int) -> Int) { }
}

protocol InoutFunctionReq {
  associatedtype T
  func updateFunction(x: inout () -> T)
}

// CHECK-LABEL: sil private [transparent] [thunk] @$s9witnesses13InoutFunctionVAA0bC3ReqA2aDP06updateC01xy1TQzycz_tFTW
// CHECK:      bb0(%0 : @trivial $*@callee_guaranteed () -> @out (), %1 : @trivial $*InoutFunction):
// CHECK-NEXT:   [[TEMP:%.*]] = alloc_stack $@callee_guaranteed () -> ()
//   Reabstract the contents of the inout argument into the temporary.
// CHECK-NEXT:   [[OLD_FN:%.*]] = load [take] %0
// CHECK-NEXT:   // function_ref
// CHECK-NEXT:   [[THUNK:%.*]] = function_ref @$sytIegr_Ieg_TR
// CHECK-NEXT:   [[THUNKED_OLD_FN:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[OLD_FN]])
// CHECK-NEXT:   store [[THUNKED_OLD_FN]] to [init] [[TEMP]] :
//   Call the function.
// CHECK-NEXT:   [[SELF:%.*]] = load [trivial] %1 : $*InoutFunction
// CHECK-NEXT:   // function_ref
// CHECK-NEXT:   [[T0:%.*]] = function_ref @$s9witnesses13InoutFunctionV06updateC01xyyycz_tF :
// CHECK-NEXT:   apply [[T0]]([[TEMP]], [[SELF]])
// CHECK-NEXT:   [[TUPLE:%.*]] = tuple ()
//   Reabstract the contents of the temporary back into the inout argument.
// CHECK-NEXT:   [[NEW_FN:%.*]] = load [take] [[TEMP]]
// CHECK-NEXT:   // function_ref
// CHECK-NEXT:   [[THUNK:%.*]] = function_ref @$sIeg_ytIegr_TR
// CHECK-NEXT:   [[THUNKED_NEW_FN:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[NEW_FN]])
// CHECK-NEXT:   store [[THUNKED_NEW_FN]] to [init] %0 :
// CHECK-NEXT:   dealloc_stack [[TEMP]]
// CHECK-NEXT:   return [[TUPLE]]
// CHECK-LABEL:  } // end sil function '$s9witnesses13InoutFunctionVAA0bC3ReqA2aDP06updateC01xy1TQzycz_tFTW'

struct InoutFunction : InoutFunctionReq {
  func updateFunction(x: inout () -> ()) {}
}


protocol Base {
  func foo()
}

protocol Sub : Base {
  func bar()
}

struct MyImpl :Sub {
  func bar() {}
	func foo() {}
}

// protocol witness for witnesses.Sub.bar() -> () in conformance witnesses.MyImpl : witnesses.Sub in witnesses
// CHECK: sil private [transparent] [thunk] @$s9witnesses6MyImplVAA3SubA2aDP3baryyFTW : $@convention(witness_method: Sub) (@in_guaranteed MyImpl) -> ()
// protocol witness for witnesses.Base.foo() -> () in conformance witnesses.MyImpl : witnesses.Base in witnesses
// CHECK: sil private [transparent] [thunk] @$s9witnesses6MyImplVAA4BaseA2aDP3fooyyFTW : $@convention(witness_method: Base) (@in_guaranteed MyImpl) -> ()
