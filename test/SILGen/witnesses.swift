// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

infix operator <~> {}

func archetype_method<T: X>(x: T, y: T) -> T {
  var x = x
  var y = y
  return x.selfTypes(x: y)
}
// CHECK-LABEL: sil hidden @_TF9witnesses16archetype_method{{.*}} : $@convention(thin) <T where T : X> (@in T, @in T) -> @out T {
// CHECK:         [[METHOD:%.*]] = witness_method $T, #X.selfTypes!1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : X> (@in τ_0_0, @inout τ_0_0) -> @out τ_0_0
// CHECK:         apply [[METHOD]]<T>({{%.*}}, {{%.*}}, {{%.*}}) : $@convention(witness_method) <τ_0_0 where τ_0_0 : X> (@in τ_0_0, @inout τ_0_0) -> @out τ_0_0
// CHECK:       }

func archetype_generic_method<T: X>(x: T, y: Loadable) -> Loadable {
  var x = x
  return x.generic(x: y)
}
// CHECK-LABEL: sil hidden @_TF9witnesses24archetype_generic_method{{.*}} : $@convention(thin) <T where T : X> (@in T, Loadable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = witness_method $T, #X.generic!1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : X><τ_1_0> (@in τ_1_0, @inout τ_0_0) -> @out τ_1_0
// CHECK:         apply [[METHOD]]<T, Loadable>({{%.*}}, {{%.*}}, {{%.*}}) : $@convention(witness_method) <τ_0_0 where τ_0_0 : X><τ_1_0> (@in τ_1_0, @inout τ_0_0) -> @out τ_1_0
// CHECK:       }

// CHECK-LABEL: sil hidden @_TF9witnesses32archetype_associated_type_method{{.*}} : $@convention(thin) <T where T : WithAssoc> (@in T, @in T.AssocType) -> @out T
// CHECK:         apply %{{[0-9]+}}<T, T.AssocType>
func archetype_associated_type_method<T: WithAssoc>(x: T, y: T.AssocType) -> T {
  return x.useAssocType(x: y)
}

protocol StaticMethod { static func staticMethod() }

// CHECK-LABEL: sil hidden @_TF9witnesses23archetype_static_method{{.*}} : $@convention(thin) <T where T : StaticMethod> (@in T) -> ()
func archetype_static_method<T: StaticMethod>(x: T) {
  // CHECK: [[METHOD:%.*]] = witness_method $T, #StaticMethod.staticMethod!1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : StaticMethod> (@thick τ_0_0.Type) -> ()
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
// CHECK-LABEL: sil hidden @_TF9witnesses15protocol_methodFT1xPS_13Existentiable__VS_8Loadable : $@convention(thin) (@in Existentiable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = witness_method $[[OPENED:@opened(.*) Existentiable]], #Existentiable.foo!1
// CHECK:         apply [[METHOD]]<[[OPENED]]>({{%.*}})
// CHECK:       }

func protocol_generic_method(x: Existentiable) -> Loadable {
  return x.generic()
}
// CHECK-LABEL: sil hidden @_TF9witnesses23protocol_generic_methodFT1xPS_13Existentiable__VS_8Loadable : $@convention(thin) (@in Existentiable) -> Loadable {
// CHECK:         [[METHOD:%.*]] = witness_method $[[OPENED:@opened(.*) Existentiable]], #Existentiable.generic!1
// CHECK:         apply [[METHOD]]<[[OPENED]], Loadable>({{%.*}}, {{%.*}})
// CHECK:       }

@objc protocol ObjCAble {
  func foo()
}

// CHECK-LABEL: sil hidden @_TF9witnesses20protocol_objc_methodFT1xPS_8ObjCAble__T_ : $@convention(thin) (@owned ObjCAble) -> ()
// CHECK:         witness_method [volatile] $@opened({{.*}}) ObjCAble, #ObjCAble.foo!1.foreign
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
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16ConformingStructS_1XS_FS1_9selfTypes{{.*}} : $@convention(witness_method) (@in ConformingStruct, @inout ConformingStruct) -> @out ConformingStruct {
  // CHECK:       bb0(%0 : $*ConformingStruct, %1 : $*ConformingStruct, %2 : $*ConformingStruct):
  // CHECK-NEXT:    %3 = load %1 : $*ConformingStruct
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %4 = function_ref @_TFV9witnesses16ConformingStruct9selfTypes{{.*}} : $@convention(method) (ConformingStruct, @inout ConformingStruct) -> ConformingStruct
  // CHECK-NEXT:    %5 = apply %4(%3, %2) : $@convention(method) (ConformingStruct, @inout ConformingStruct) -> ConformingStruct
  // CHECK-NEXT:    store %5 to %0 : $*ConformingStruct
  // CHECK-NEXT:    %7 = tuple ()
  // CHECK-NEXT:    return %7 : $()
  // CHECK-NEXT:  }
  
  mutating
  func loadable(x: Loadable) -> Loadable { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16ConformingStructS_1XS_FS1_8loadable{{.*}} : $@convention(witness_method) (Loadable, @inout ConformingStruct) -> Loadable {
  // CHECK:       bb0(%0 : $Loadable, %1 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %2 = function_ref @_TFV9witnesses16ConformingStruct8loadable{{.*}} : $@convention(method) (Loadable, @inout ConformingStruct) -> Loadable
  // CHECK-NEXT:    %3 = apply %2(%0, %1) : $@convention(method) (Loadable, @inout ConformingStruct) -> Loadable
  // CHECK-NEXT:    return %3 : $Loadable
  // CHECK-NEXT:  }
  
  mutating
  func addrOnly(x: AddrOnly) -> AddrOnly { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16ConformingStructS_1XS_FS1_8addrOnly{{.*}} : $@convention(witness_method) (@in AddrOnly, @inout ConformingStruct) -> @out AddrOnly {
  // CHECK:       bb0(%0 : $*AddrOnly, %1 : $*AddrOnly, %2 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses16ConformingStruct8addrOnly{{.*}} : $@convention(method) (@in AddrOnly, @inout ConformingStruct) -> @out AddrOnly
  // CHECK-NEXT:    %4 = apply %3(%0, %1, %2) : $@convention(method) (@in AddrOnly, @inout ConformingStruct) -> @out AddrOnly
  // CHECK-NEXT:    %5 = tuple ()
  // CHECK-NEXT:    return %5 : $()
  // CHECK-NEXT:  }
  
  mutating
  func generic<C>(x: C) -> C { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16ConformingStructS_1XS_FS1_7generic{{.*}} : $@convention(witness_method) <A> (@in A, @inout ConformingStruct) -> @out A {
  // CHECK:       bb0(%0 : $*A, %1 : $*A, %2 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses16ConformingStruct7generic{{.*}} : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformingStruct) -> @out τ_0_0
  // CHECK-NEXT:    %4 = apply %3<A>(%0, %1, %2) : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformingStruct) -> @out τ_0_0
  // CHECK-NEXT:    %5 = tuple ()
  // CHECK-NEXT:    return %5 : $()
  // CHECK-NEXT:  }
  mutating
  func classes<C2: Classes>(x: C2) -> C2 { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16ConformingStructS_1XS_FS1_7classes{{.*}} : $@convention(witness_method) <A2 where A2 : Classes> (@owned A2, @inout ConformingStruct) -> @owned A2 {
  // CHECK:       bb0(%0 : $A2, %1 : $*ConformingStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %2 = function_ref @_TFV9witnesses16ConformingStruct7classes{{.*}} : $@convention(method) <τ_0_0 where τ_0_0 : Classes> (@owned τ_0_0, @inout ConformingStruct) -> @owned τ_0_0
  // CHECK-NEXT:    %3 = apply %2<A2>(%0, %1) : $@convention(method) <τ_0_0 where τ_0_0 : Classes> (@owned τ_0_0, @inout ConformingStruct) -> @owned τ_0_0
  // CHECK-NEXT:    return %3 : $A2
  // CHECK-NEXT:  }
}
func <~>(_ x: ConformingStruct, y: ConformingStruct) -> ConformingStruct { return x }
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16ConformingStructS_1XS_ZFS1_oi3ltg{{.*}} : $@convention(witness_method) (@in ConformingStruct, @in ConformingStruct, @thick ConformingStruct.Type) -> @out ConformingStruct {
// CHECK:       bb0(%0 : $*ConformingStruct, %1 : $*ConformingStruct, %2 : $*ConformingStruct, %3 : $@thick ConformingStruct.Type):
// CHECK-NEXT:    %4 = load %1 : $*ConformingStruct
// CHECK-NEXT:    %5 = load %2 : $*ConformingStruct
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    %6 = function_ref @_TF9witnessesoi3ltgFTVS_16ConformingStructS0__S0_ : $@convention(thin) (ConformingStruct, ConformingStruct) -> ConformingStruct
// CHECK-NEXT:    %7 = apply %6(%4, %5) : $@convention(thin) (ConformingStruct, ConformingStruct) -> ConformingStruct
// CHECK-NEXT:    store %7 to %0 : $*ConformingStruct
// CHECK-NEXT:    %9 = tuple ()
// CHECK-NEXT:    return %9 : $()
// CHECK-NEXT:  }

final class ConformingClass : X {
  func selfTypes(x: ConformingClass) -> ConformingClass { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses15ConformingClassS_1XS_FS1_9selfTypes{{.*}} : $@convention(witness_method) (@in ConformingClass, @inout ConformingClass) -> @out ConformingClass {
  // CHECK:       bb0(%0 : $*ConformingClass, %1 : $*ConformingClass, %2 : $*ConformingClass):
  // -- load and retain 'self' from inout witness 'self' parameter
  // CHECK-NEXT:    %3 = load %2 : $*ConformingClass
  // CHECK-NEXT:    strong_retain %3 : $ConformingClass
  // CHECK-NEXT:    %5 = load %1 : $*ConformingClass
  // CHECK:         %6 = function_ref @_TFC9witnesses15ConformingClass9selfTypes 
  // CHECK-NEXT:    %7 = apply %6(%5, %3) : $@convention(method) (@owned ConformingClass, @guaranteed ConformingClass) -> @owned ConformingClass
  // CHECK-NEXT:    store %7 to %0 : $*ConformingClass
  // CHECK-NEXT:    %9 = tuple ()
  // CHECK-NEXT:    strong_release %3
  // CHECK-NEXT:    return %9 : $()
  // CHECK-NEXT:  }
  func loadable(x: Loadable) -> Loadable { return x }
  func addrOnly(x: AddrOnly) -> AddrOnly { return x }
  func generic<D>(x: D) -> D { return x }
  func classes<D2: Classes>(x: D2) -> D2 { return x }
}
func <~>(_ x: ConformingClass, y: ConformingClass) -> ConformingClass { return x }

extension ConformingClass : ClassBounded { }
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses15ConformingClassS_12ClassBoundedS_FS1_9selfTypes{{.*}} : $@convention(witness_method) (@owned ConformingClass, @guaranteed ConformingClass) -> @owned ConformingClass {
// CHECK:  bb0([[C0:%.*]] : $ConformingClass, [[C1:%.*]] : $ConformingClass):
// CHECK-NEXT:    strong_retain [[C1]]
// CHECK-NEXT:    function_ref
// CHECK-NEXT:    [[FUN:%.*]] = function_ref @_TFC9witnesses15ConformingClass9selfTypes
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[FUN]]([[C0]], [[C1]]) : $@convention(method) (@owned ConformingClass, @guaranteed ConformingClass) -> @owned ConformingClass
// CHECK-NEXT:    strong_release [[C1]]
// CHECK-NEXT:    return [[RESULT]] : $ConformingClass
// CHECK-NEXT:  }

struct ConformingAOStruct : X {
  var makeMeAO : AddrOnly

  mutating
  func selfTypes(x: ConformingAOStruct) -> ConformingAOStruct { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses18ConformingAOStructS_1XS_FS1_9selfTypes{{.*}} : $@convention(witness_method) (@in ConformingAOStruct, @inout ConformingAOStruct) -> @out ConformingAOStruct {
  // CHECK:       bb0(%0 : $*ConformingAOStruct, %1 : $*ConformingAOStruct, %2 : $*ConformingAOStruct):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses18ConformingAOStruct9selfTypes{{.*}} : $@convention(method) (@in ConformingAOStruct, @inout ConformingAOStruct) -> @out ConformingAOStruct
  // CHECK-NEXT:    %4 = apply %3(%0, %1, %2) : $@convention(method) (@in ConformingAOStruct, @inout ConformingAOStruct) -> @out ConformingAOStruct
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
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_FS1_9selfTypes{{.*}} : $@convention(witness_method) (@in ConformsWithMoreGeneric, @inout ConformsWithMoreGeneric) -> @out ConformsWithMoreGeneric {
  // CHECK:       bb0(%0 : $*ConformsWithMoreGeneric, %1 : $*ConformsWithMoreGeneric, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    [[WITNESS_FN:%.*]] = function_ref @_TFV9witnesses23ConformsWithMoreGeneric9selfTypes{{.*}} : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = apply [[WITNESS_FN]]<ConformsWithMoreGeneric>(%0, %1, %2) : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RESULT]] : $()
  // CHECK-NEXT:  }
  func loadable<F>(x: F) -> F { return x }
  mutating
  func addrOnly<G>(x: G) -> G { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_FS1_8addrOnly{{.*}} : $@convention(witness_method) (@in AddrOnly, @inout ConformsWithMoreGeneric) -> @out AddrOnly {
  // CHECK:       bb0(%0 : $*AddrOnly, %1 : $*AddrOnly, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses23ConformsWithMoreGeneric8addrOnly{{.*}} : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    %4 = apply %3<AddrOnly>(%0, %1, %2) : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RESULT]] : $()
  // CHECK-NEXT:  }

  mutating
  func generic<H>(x: H) -> H { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_FS1_7generic{{.*}} : $@convention(witness_method) <A> (@in A, @inout ConformsWithMoreGeneric) -> @out A {
  // CHECK:       bb0(%0 : $*A, %1 : $*A, %2 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    %3 = function_ref @_TFV9witnesses23ConformsWithMoreGeneric7generic{{.*}} : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    %4 = apply %3<A>(%0, %1, %2) : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RESULT]] : $()
  // CHECK-NEXT:  }

  mutating
  func classes<I>(x: I) -> I { return x }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_FS1_7classes{{.*}} : $@convention(witness_method) <A2 where A2 : Classes> (@owned A2, @inout ConformsWithMoreGeneric) -> @owned A2 {
  // CHECK:       bb0(%0 : $A2, %1 : $*ConformsWithMoreGeneric):
  // CHECK-NEXT:    [[SELF_BOX:%.*]] = alloc_stack $A2
  // CHECK-NEXT:    store %0 to [[SELF_BOX]] : $*A2
  // CHECK-NEXT:    // function_ref witnesses.ConformsWithMoreGeneric.classes
  // CHECK-NEXT:    [[WITNESS_FN:%.*]] = function_ref @_TFV9witnesses23ConformsWithMoreGeneric7classes{{.*}} : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT_BOX:%.*]] = alloc_stack $A2
  // CHECK-NEXT:    [[RESULT:%.*]] = apply [[WITNESS_FN]]<A2>([[RESULT_BOX]], [[SELF_BOX]], %1) : $@convention(method) <τ_0_0> (@in τ_0_0, @inout ConformsWithMoreGeneric) -> @out τ_0_0
  // CHECK-NEXT:    [[RESULT:%.*]] = load [[RESULT_BOX]] : $*A2
  // CHECK-NEXT:    dealloc_stack [[RESULT_BOX]] : $*A2
  // CHECK-NEXT:    dealloc_stack [[SELF_BOX]] : $*A2
  // CHECK-NEXT:    return [[RESULT]] : $A2
  // CHECK-NEXT:  }
}
func <~> <J: Y, K: Y>(_ x: J, y: K) -> K { return y }
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses23ConformsWithMoreGenericS_1XS_ZFS1_oi3ltg{{.*}} : $@convention(witness_method) (@in ConformsWithMoreGeneric, @in ConformsWithMoreGeneric, @thick ConformsWithMoreGeneric.Type) -> @out ConformsWithMoreGeneric {
// CHECK:       bb0(%0 : $*ConformsWithMoreGeneric, %1 : $*ConformsWithMoreGeneric, %2 : $*ConformsWithMoreGeneric, %3 : $@thick ConformsWithMoreGeneric.Type):
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[WITNESS_FN:%.*]] = function_ref @_TF9witnessesoi3ltg{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Y, τ_0_1 : Y> (@in τ_0_0, @in τ_0_1) -> @out τ_0_1
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[WITNESS_FN]]<ConformsWithMoreGeneric, ConformsWithMoreGeneric>(%0, %1, %2) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Y, τ_0_1 : Y> (@in τ_0_0, @in τ_0_1) -> @out τ_0_1
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:    return [[RESULT]] : $()
// CHECK-NEXT:  }

protocol LabeledRequirement {
  func method(x: Loadable)
}

struct UnlabeledWitness : LabeledRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16UnlabeledWitnessS_18LabeledRequirementS_FS1_6method{{.*}} : $@convention(witness_method) (Loadable, @in_guaranteed UnlabeledWitness) -> ()
  func method(x _: Loadable) {}
}

protocol LabeledSelfRequirement {
  func method(x: Self)
}

struct UnlabeledSelfWitness : LabeledSelfRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses20UnlabeledSelfWitnessS_22LabeledSelfRequirementS_FS1_6method{{.*}} : $@convention(witness_method) (@in UnlabeledSelfWitness, @in_guaranteed UnlabeledSelfWitness) -> ()
  func method(x _: UnlabeledSelfWitness) {}
}

protocol UnlabeledRequirement {
  func method(x _: Loadable)
}

struct LabeledWitness : UnlabeledRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses14LabeledWitnessS_20UnlabeledRequirementS_FS1_6method{{.*}} : $@convention(witness_method) (Loadable, @in_guaranteed LabeledWitness) -> ()
  func method(x: Loadable) {}
}

protocol UnlabeledSelfRequirement {
  func method(_: Self)
}

struct LabeledSelfWitness : UnlabeledSelfRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses18LabeledSelfWitnessS_24UnlabeledSelfRequirementS_FS1_6method{{.*}} : $@convention(witness_method) (@in LabeledSelfWitness, @in_guaranteed LabeledSelfWitness) -> ()
  func method(_ x: LabeledSelfWitness) {}
}

protocol ReadOnlyRequirement {
  var prop: String { get }
  static var prop: String { get }
}

struct ImmutableModel: ReadOnlyRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses14ImmutableModelS_19ReadOnlyRequirementS_FS1_g4propSS : $@convention(witness_method) (@in_guaranteed ImmutableModel) -> @owned String
  let prop: String = "a"
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses14ImmutableModelS_19ReadOnlyRequirementS_ZFS1_g4propSS : $@convention(witness_method) (@thick ImmutableModel.Type) -> @owned String
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
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16NonFailableModelS_19FailableRequirementS_FS1_C{{.*}} : $@convention(witness_method) (Int, @thick NonFailableModel.Type) -> @out Optional<NonFailableModel>
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16NonFailableModelS_21NonFailableRefinementS_FS1_C{{.*}} : $@convention(witness_method) (Int, @thick NonFailableModel.Type) -> @out NonFailableModel
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16NonFailableModelS_22IUOFailableRequirementS_FS1_C{{.*}} : $@convention(witness_method) (Int, @thick NonFailableModel.Type) -> @out ImplicitlyUnwrappedOptional<NonFailableModel>
  init(foo: Int) {}
}

struct FailableModel: FailableRequirement, IUOFailableRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses13FailableModelS_19FailableRequirementS_FS1_C{{.*}}

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses13FailableModelS_22IUOFailableRequirementS_FS1_C{{.*}}
  // CHECK: bb0([[SELF:%[0-9]+]] : $*ImplicitlyUnwrappedOptional<FailableModel>, [[FOO:%[0-9]+]] : $Int, [[META:%[0-9]+]] : $@thick FailableModel.Type):
  // CHECK: [[FN:%.*]] = function_ref @_TFV9witnesses13FailableModelC{{.*}}
  // CHECK: [[INNER:%.*]] = apply [[FN]](
  // CHECK: [[OUTER:%.*]] = unchecked_trivial_bit_cast [[INNER]] : $Optional<FailableModel> to $ImplicitlyUnwrappedOptional<FailableModel>
  // CHECK: store [[OUTER]] to %0
  // CHECK: return
  init?(foo: Int) {}
}

struct IUOFailableModel : NonFailableRefinement, IUOFailableRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9witnesses16IUOFailableModelS_21NonFailableRefinementS_FS1_C{{.*}}
  // CHECK: bb0([[SELF:%[0-9]+]] : $*IUOFailableModel, [[FOO:%[0-9]+]] : $Int, [[META:%[0-9]+]] : $@thick IUOFailableModel.Type):
  // CHECK:   [[META:%[0-9]+]] = metatype $@thin IUOFailableModel.Type
  // CHECK:   [[INIT:%[0-9]+]] = function_ref @_TFV9witnesses16IUOFailableModelC{{.*}} : $@convention(method) (Int, @thin IUOFailableModel.Type) -> ImplicitlyUnwrappedOptional<IUOFailableModel>
  // CHECK:   [[IUO_RESULT:%[0-9]+]] = apply [[INIT]]([[FOO]], [[META]]) : $@convention(method) (Int, @thin IUOFailableModel.Type) -> ImplicitlyUnwrappedOptional<IUOFailableModel>
  // CHECK:   [[RESULT:%[0-9]+]] = unchecked_enum_data [[IUO_RESULT]]
  // CHECK:   store [[RESULT]] to [[SELF]] : $*IUOFailableModel
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
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses21NonFailableClassModelS_24FailableClassRequirementS_FS1_C{{.*}} : $@convention(witness_method) (Int, @thick NonFailableClassModel.Type) -> @owned Optional<NonFailableClassModel>
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses21NonFailableClassModelS_26NonFailableClassRefinementS_FS1_C{{.*}} : $@convention(witness_method) (Int, @thick NonFailableClassModel.Type) -> @owned NonFailableClassModel
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses21NonFailableClassModelS_27IUOFailableClassRequirementS_FS1_C{{.*}} : $@convention(witness_method) (Int, @thick NonFailableClassModel.Type) -> @owned ImplicitlyUnwrappedOptional<NonFailableClassModel>
  init(foo: Int) {}
}

final class FailableClassModel: FailableClassRequirement, IUOFailableClassRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses18FailableClassModelS_24FailableClassRequirementS_FS1_C{{.*}}

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses18FailableClassModelS_27IUOFailableClassRequirementS_FS1_C{{.*}}
  // CHECK: [[FUNC:%.*]] = function_ref @_TFC9witnesses18FailableClassModelC{{.*}}
  // CHECK: [[INNER:%.*]] = apply [[FUNC]](%0, %1)
  // CHECK: [[OUTER:%.*]] = unchecked_ref_cast [[INNER]] : $Optional<FailableClassModel> to $ImplicitlyUnwrappedOptional<FailableClassModel>
  // CHECK: return [[OUTER]] : $ImplicitlyUnwrappedOptional<FailableClassModel>
  init?(foo: Int) {}
}

final class IUOFailableClassModel: NonFailableClassRefinement, IUOFailableClassRequirement {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses21IUOFailableClassModelS_26NonFailableClassRefinementS_FS1_C{{.*}}
  // CHECK: unchecked_enum_data
  // CHECK: return [[RESULT:%[0-9]+]] : $IUOFailableClassModel

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses21IUOFailableClassModelS_27IUOFailableClassRequirementS_FS1_C{{.*}}
  init!(foo: Int) {}

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses21IUOFailableClassModelS_24FailableClassRequirementS_FS1_C{{.*}}
  // CHECK: [[FUNC:%.*]] = function_ref @_TFC9witnesses21IUOFailableClassModelC{{.*}}
  // CHECK: [[INNER:%.*]] = apply [[FUNC]](%0, %1)
  // CHECK: [[OUTER:%.*]] = unchecked_ref_cast [[INNER]] : $ImplicitlyUnwrappedOptional<IUOFailableClassModel> to $Optional<IUOFailableClassModel>
  // CHECK: return [[OUTER]] : $Optional<IUOFailableClassModel>
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

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTW{{.*}}GenericParameterNameCollision{{.*}}GenericParameterNameCollisionProtocol{{.*}}foo{{.*}} : $@convention(witness_method) <T1 where T1 : HasAssoc><T> (@in T, @in_guaranteed GenericParameterNameCollision<T1>) -> () {
  // CHECK:       bb0(%0 : $*T, %1 : $*GenericParameterNameCollision<T1>):
  // CHECK:         apply {{%.*}}<T1, T1.Assoc, T>
  func foo<U>(_ x: U) {}

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTW{{.*}}GenericParameterNameCollision{{.*}}GenericParameterNameCollisionProtocol{{.*}}bar{{.*}} : $@convention(witness_method) <T1 where T1 : HasAssoc><T> (@owned @callee_owned (@in T) -> @out T1.Assoc, @in_guaranteed GenericParameterNameCollision<T1>) -> () {
  // CHECK:       bb0(%0 : $@callee_owned (@in T) -> @out T1.Assoc, %1 : $*GenericParameterNameCollision<T1>):
  // CHECK:         apply {{%.*}}<T1, T1.Assoc, T>
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

  // Make sure the contravariant return type in materializeForSet works correctly

  // If the witness is in a base class of the conforming class, make sure we have a bit_cast in there:

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses34PropertyRequirementWitnessFromBaseS_19PropertyRequirementS_FS1_m5widthSi : {{.*}} {
  // CHECK: upcast
  // CHECK-NEXT: [[METH:%.*]] = class_method {{%.*}} : $PropertyRequirementBase, #PropertyRequirementBase.width!materializeForSet.1
  // CHECK-NEXT: [[RES:%.*]] = apply [[METH]]
  // CHECK-NEXT: [[CAR:%.*]] = tuple_extract [[RES]] : $({{.*}}), 0
  // CHECK-NEXT: [[CADR:%.*]] = tuple_extract [[RES]] : $({{.*}}), 1
  // CHECK-NEXT: [[TUPLE:%.*]] = tuple ([[CAR]] : {{.*}}, [[CADR]] : {{.*}})
  // CHECK-NEXT: strong_release
  // CHECK-NEXT: return [[TUPLE]]

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses34PropertyRequirementWitnessFromBaseS_19PropertyRequirementS_ZFS1_m6heightSi : {{.*}} {
  // CHECK: [[OBJ:%.*]] = upcast %2 : $@thick PropertyRequirementWitnessFromBase.Type to $@thick PropertyRequirementBase.Type
  // CHECK: [[METH:%.*]] = function_ref @_TZFC9witnesses23PropertyRequirementBasem6heightSi
  // CHECK-NEXT: [[RES:%.*]] = apply [[METH]]
  // CHECK-NEXT: [[CAR:%.*]] = tuple_extract [[RES]] : $({{.*}}), 0
  // CHECK-NEXT: [[CADR:%.*]] = tuple_extract [[RES]] : $({{.*}}), 1
  // CHECK-NEXT: [[TUPLE:%.*]] = tuple ([[CAR]] : {{.*}}, [[CADR]] : {{.*}})
  // CHECK-NEXT: return [[TUPLE]]

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9witnesses34PropertyRequirementWitnessFromBaseS_19PropertyRequirementS_FS1_m5depthSi
  // CHECK: [[METH:%.*]] = class_method {{%.*}} : $PropertyRequirementWitnessFromBase, #PropertyRequirementWitnessFromBase.depth!materializeForSet.1
  // CHECK-NEXT: [[RES:%.*]] = apply [[METH]]
  // CHECK-NEXT: tuple_extract
  // CHECK-NEXT: tuple_extract
  // CHECK-NEXT: [[RES:%.*]] = tuple
  // CHECK-NEXT: strong_release
  // CHECK-NEXT: return [[RES]]
}
