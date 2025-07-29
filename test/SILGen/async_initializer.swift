// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name initializers -swift-version 5  -target %target-swift-5.1-abi-triple | %FileCheck -check-prefix=CHECK -check-prefix=NI %s --enable-var-scope --implicit-check-not=hop_to_executor
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name initializers -swift-version 5  -target %target-swift-5.1-abi-triple -enable-upcoming-feature NonisolatedNonsendingByDefault | %FileCheck %s -check-prefix=CHECK -check-prefix=NI-NS --enable-var-scope --implicit-check-not=hop_to_executor

// REQUIRES: concurrency
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// CHECK:       protocol Person {
// CHECK-NEXT:    init() async
// CHECK-NEXT:  }

// CHECK:       struct MyStruct {
// CHECK-NEXT:    init() async
// CHECK-NEXT:  }

// CHECK:       enum MyEnum {
// CHECK-NEXT:    init() async
// CHECK-NEXT:  }

// CHECK:       actor MyActor {
// CHECK-NEXT:    init() async


// CHECK:       class EarthPerson : Person {
// CHECK-NEXT:    required {{.*}}init() async
// CHECK-NEXT:    init(name: String) async

// CHECK:       @_inheritsConvenienceInitializers class NorthAmericaPerson : EarthPerson {
// CHECK-NEXT:    required {{.*}}init() async
// CHECK-NEXT:    override {{.*}}init(name: String) async

// CHECK:       class Cat {
// CHECK-NEXT:    @MainActor init()
// CHECK-NEXT:    @MainActor init(name: String)

// CHECK:       struct Dog {
// CHECK-NEXT:    @MainActor init() async
// CHECK-NEXT:    @MainActor init(name: String)
// CHECK-NEXT:  }

// CHECK:       enum Birb {
// CHECK-NEXT:    @MainActor init() async
// CHECK-NEXT:    @MainActor init(name: String)
// CHECK-NEXT:  }



protocol Person {
  init() async
}

struct MyStruct {
  // NI-DAG: sil hidden [ossa] @$s12initializers8MyStructVACyYacfC : $@convention(method) @async (@thin MyStruct.Type) -> MyStruct
  // NI-NS-DAG: sil hidden [ossa] @$s12initializers8MyStructVACyYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thin MyStruct.Type) -> MyStruct {
  init() async {}
}

enum MyEnum {
  // NI-DAG: sil hidden [ossa] @$s12initializers6MyEnumOACyYacfC : $@convention(method) @async (@thin MyEnum.Type) -> MyEnum
  // NI-NS-DAG: sil hidden [ossa] @$s12initializers6MyEnumOACyYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thin MyEnum.Type) -> MyEnum {
  init() async {}
}

actor MyActor {
  // CHECK-DAG:   sil hidden [ossa] @$s12initializers7MyActorCACyYacfc : $@convention(method) @async (@sil_isolated @owned MyActor) -> @owned MyActor
  // CHECK:       bb0(%0 : @owned $MyActor):
  //   In the prologue, hop to the generic executor.
  // CHECK-NEXT:       [[NIL_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK-NEXT:       hop_to_executor [[NIL_EXECUTOR]] :
  //   Later, when we return from an async call, hop to the
  //   correct flow-sensitive value.
  //
  // NI:         [[FN:%.*]] = function_ref @$s12initializers8MyStructVACyYacfC :
  // NI-NEXT:    apply [[FN]]
  //
  // NI-NS:      [[ISOLATION_1:%.*]] = builtin "flowSensitiveSelfIsolation"<MyActor>({{%.*}})
  // NI-NS-NEXT: // function_ref
  // NI-NS-NEXT: [[FN:%.*]] = function_ref @$s12initializers8MyStructVACyYacfC :
  // NI-NS-NEXT: apply [[FN]]([[ISOLATION_1]]
  // NI-NS-NEXT: destroy_value [[ISOLATION_1]]
  // CHECK-NEXT:       [[ISOLATION_2:%.*]] = builtin "flowSensitiveSelfIsolation"<MyActor>({{%.*}})
  // CHECK-NEXT:       hop_to_executor [[ISOLATION_2]]
  // CHECK: } // end sil function '$s12initializers7MyActorCACyYacfc'
  init() async {
    _ = await MyStruct()
  }
}

class EarthPerson : Person {
  // NI-NS-LABEL: sil hidden [exact_self_class] [ossa] @$s12initializers11EarthPersonCACyYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thick EarthPerson.Type) -> @owned EarthPerson {
  // NI-NS: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
  // NI-NS: hop_to_executor [[ACTOR]]
  // NI-NS: apply {{%.*}}([[ACTOR]], {{%.*}})
  // NI-NS: } // end sil function '$s12initializers11EarthPersonCACyYacfC'
  

  // NI-DAG: sil hidden [ossa] @$s12initializers11EarthPersonCACyYacfc : $@convention(method) @async (@owned EarthPerson) -> @owned EarthPerson
  // NI-NS-DAG: sil hidden [ossa] @$s12initializers11EarthPersonCACyYacfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @owned EarthPerson) -> @owned EarthPerson {
  required init() async {}

  // NI-DAG: sil hidden [ossa] @$s12initializers11EarthPersonC4nameACSS_tYacfc : $@convention(method) @async (@owned String, @owned EarthPerson) -> @owned EarthPerson
  // NI-NS-DAG: sil hidden [ossa] @$s12initializers11EarthPersonC4nameACSS_tYacfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @owned String, @owned EarthPerson) -> @owned EarthPerson {
  init(name: String) async {}

  // NI-DAG: sil private [transparent] [thunk] [ossa] @$s12initializers11EarthPersonCAA0C0A2aDPxyYacfCTW : $@convention(
  // NI-NS-DAG: sil private [transparent] [thunk] [ossa] @$s12initializers11EarthPersonCAA0C0A2aDPxyYacfCTW : $@convention(witness_method: Person) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thick EarthPerson.Type) -> @out EarthPerson {
}

class NorthAmericaPerson : EarthPerson {
  // NI-DAG: sil hidden [ossa] @$s12initializers18NorthAmericaPersonCACyYacfc : $@convention(method) @async (@owned NorthAmericaPerson) -> @owned NorthAmericaPerson
  // NI-NS-DAG: sil hidden [ossa] @$s12initializers18NorthAmericaPersonCACyYacfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @owned NorthAmericaPerson) -> @owned NorthAmericaPerson {
  required init() async { await super.init() }

  // NI-DAG: sil hidden [ossa] @$s12initializers18NorthAmericaPersonC4nameACSS_tYacfc : $@convention(method) @async (@owned String, @owned NorthAmericaPerson) -> @owned NorthAmericaPerson
  // NI-NS-DAG: sil hidden [ossa] @$s12initializers18NorthAmericaPersonC4nameACSS_tYacfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @owned String, @owned NorthAmericaPerson) -> @owned NorthAmericaPerson {
  override init(name: String) async { await super.init(name: name) }
}

////
// check that of global-actor isolation is preserved in async initializers.
// there's also coverage for sync global-actor-isolated initializers

func someAsyncFn() async {}

class Cat {
  // CHECK-LABEL:  sil hidden [ossa] @$s12initializers3CatCACyYacfc : $@convention(method) @async (@owned Cat) -> @owned Cat {
  // CHECK:          hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:          {{%[0-9]+}} = function_ref @$s12initializers11someAsyncFnyyYaF :
  // NI:             {{%[0-9]+}} = apply {{%[0-9]+}}() : $@convention(thin) @async () -> ()
  // NI-NS:          {{%[0-9]+}} = apply {{%[0-9]+}}({{%.*}}) : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
  // NI-NS-NEXT:     destroy_value {{%.*}}
  // CHECK-NEXT:     hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:        } // end sil function '$s12initializers3CatCACyYacfc'
  @MainActor init() async {
    await someAsyncFn()
  }

  @MainActor init(name: String) {}
}

struct Dog {
  // CHECK-LABEL:  sil hidden [ossa] @$s12initializers3DogVACyYacfC : $@convention(method) @async (@thin Dog.Type) -> Dog {
  // CHECK:          hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:          {{%[0-9]+}} = function_ref @$s12initializers11someAsyncFnyyYaF
  // NI:             {{%[0-9]+}} = apply {{%[0-9]+}}() : $@convention(thin) @async () -> ()
  // NI-NS:          {{%[0-9]+}} = apply {{%[0-9]+}}({{%.*}}) : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
  // NI-NS-NEXT:     destroy_value {{%.*}}
  // CHECK-NEXT:     hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:        } // end sil function '$s12initializers3DogVACyYacfC'
  @MainActor init() async {
    await someAsyncFn()
  }

  @MainActor init(name: String) {}
}

enum Birb {
  // CHECK-LABEL:  sil hidden [ossa] @$s12initializers4BirbOACyYacfC : $@convention(method) @async (@thin Birb.Type) -> Birb {
  // CHECK:          hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:          {{%[0-9]+}} = function_ref @$s12initializers11someAsyncFnyyYaF :
  // NI:             {{%[0-9]+}} = apply {{%[0-9]+}}() : $@convention(thin) @async () -> ()
  // NI-NS:          {{%[0-9]+}} = apply {{%[0-9]+}}({{%.*}}) :
  // CHECK:          hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:        } // end sil function '$s12initializers4BirbOACyYacfC'
  @MainActor init() async {
    await someAsyncFn()
  }

  @MainActor init(name: String) {}
}

// NI-LABEL:  sil hidden [ossa] @$s12initializers7makeCatyyYaF : $@convention(thin) @async () -> () {
// NI:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:          hop_to_executor [[BORROWED_EXECUTOR:%[0-9]+]]
// NI:          end_borrow [[BORROWED_EXECUTOR]]
// NI-NEXT:     {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thick Cat.Type) -> @owned Cat
// NI:          hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:        } // end sil function '$s12initializers7makeCatyyYaF'

// NI-NS-LABEL:  sil hidden [ossa] @$s12initializers7makeCatyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// NI-NS:        bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// NI-NS:          hop_to_executor [[ACTOR]]
// NI-NS:          [[INIT:%.*]] = function_ref @$s12initializers3CatC4nameACSS_tcfC : $@convention(method) (@owned String, @thick Cat.Type) -> @owned Cat
// NI-NS:          [[MAIN_ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// NI-NS:          [[MAIN_ACTOR_B:%.*]] = begin_borrow [[MAIN_ACTOR]]
// NI-NS:          hop_to_executor [[MAIN_ACTOR_B]]
// NI-NS:          {{%[0-9]+}} = apply [[INIT]]({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thick Cat.Type) -> @owned Cat
// NI-NS:          hop_to_executor [[ACTOR]]
// NI-NS:        } // end sil function '$s12initializers7makeCatyyYaF'
func makeCat() async {
  _ = await Cat(name: "Socks")
}

// NI-LABEL:  sil hidden [ossa] @$s12initializers7makeDogyyYaF : $@convention(thin) @async () -> () {
// NI:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:          hop_to_executor [[BORROWED_EXEC:%.*]] :
// NI-NEXT:     end_borrow [[BORROWED_EXEC]]
// NI-NEXT:     {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thin Dog.Type) -> Dog
// NI:          hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:        } // end sil function '$s12initializers7makeDogyyYaF'

// NI-NS-LABEL: sil hidden [ossa] @$s12initializers7makeDogyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// NI-NS: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// NI-NS:      hop_to_executor [[ACTOR]]
// NI-NS:      hop_to_executor [[BORROWED_EXEC:%.*]] :
// NI-NS-NEXT: end_borrow [[BORROWED_EXEC]]
// NI-NS-NEXT: {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thin Dog.Type) -> Dog
// NI-NS:      hop_to_executor [[ACTOR]]
// NI-NS: } // end sil function '$s12initializers7makeDogyyYaF'
func makeDog() async {
  _ = await Dog(name: "Lassie")
}

// NI-LABEL:  sil hidden [ossa] @$s12initializers8makeBirbyyYaF : $@convention(thin) @async () -> () {
// NI:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:          hop_to_executor [[BORROWED_EXEC:%.*]] : $MainActor
// NI-NEXT:     end_borrow [[BORROWED_EXEC]]
// NI-NEXT:     {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thin Birb.Type) -> Birb
// NI:          hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:        } // end sil function '$s12initializers8makeBirbyyYaF'

// NI-NS-LABEL:  sil hidden [ossa] @$s12initializers8makeBirbyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// NI-NS:        bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// NI-NS-NEXT:     hop_to_executor [[ACTOR]]
// NI-NS:          hop_to_executor [[BORROWED_EXEC:%.*]] : $MainActor
// NI-NS-NEXT:     end_borrow [[BORROWED_EXEC]]
// NI-NS-NEXT:     {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thin Birb.Type) -> Birb
// NI-NS:          hop_to_executor [[ACTOR]]
// NI-NS:        } // end sil function '$s12initializers8makeBirbyyYaF'
func makeBirb() async {
  _ = await Birb(name: "Chirpy")
}

actor SomeActor {
  var x: Int = 0

  // CHECK-LABEL: sil hidden [ossa] @$s12initializers9SomeActorCACyYacfc : $@convention(method) @async (@sil_isolated @owned SomeActor) -> @owned SomeActor {
  // CHECK:       bb0(%0 :
  // CHECK-NEXT:    [[NIL_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK-NEXT:    hop_to_executor [[NIL_EXECUTOR]]
  init() async {}

  // CHECK-LABEL: sil hidden [ossa] @$s12initializers9SomeActorC10someMethodyyYaF : $@convention(method) @async (@sil_isolated @guaranteed SomeActor) -> () {
  // CHECK:           hop_to_executor {{%[0-9]+}} : $SomeActor
  // CHECK: } // end sil function '$s12initializers9SomeActorC10someMethodyyYaF'
  func someMethod() async {}
}

// CHECK-LABEL: sil hidden [ossa] @$s12initializers9makeActorAA04SomeC0CyYaF : $@convention(thin) @async () -> @owned SomeActor {
// CHECK:         hop_to_executor {{%[0-9]+}} : $MainActor
// CHECK:         [[INIT:%[0-9]+]] = function_ref @$s12initializers9SomeActorCACyYacfC : $@convention(method) @async (@thick SomeActor.Type) -> @owned SomeActor
// CHECK:         {{%[0-9]+}} = apply [[INIT]]({{%[0-9]+}}) : $@convention(method) @async (@thick SomeActor.Type) -> @owned SomeActor
// CHECK:         hop_to_executor {{%[0-9]+}} : $MainActor
// CHECK: } // end sil function '$s12initializers9makeActorAA04SomeC0CyYaF'
@MainActor
func makeActor() async -> SomeActor {
  return await SomeActor()
}

// NI-LABEL: sil hidden [ossa] @$s12initializers20makeActorFromGenericAA04SomeC0CyYaF : $@convention(thin) @async () -> @owned SomeActor {
// NI:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:          apply
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI: } // end sil function '$s12initializers20makeActorFromGenericAA04SomeC0CyYaF'

// NI-NS-LABEL: sil hidden [ossa] @$s12initializers20makeActorFromGenericAA04SomeC0CyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> @owned SomeActor {
// NI-NS:      bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// NI-NS:          hop_to_executor [[ACTOR]]
// NI-NS:          apply {{%.*}}({{%.*}})
// NI-NS-NEXT:     hop_to_executor [[ACTOR]]
// NI-NS: } // end sil function '$s12initializers20makeActorFromGenericAA04SomeC0CyYaF'
func makeActorFromGeneric() async -> SomeActor {
  return await SomeActor()
}

// NI-LABEL: sil hidden [ossa] @$s12initializers26callActorMethodFromGeneric1ayAA04SomeC0C_tYaF :
// NI:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:          apply
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI: } // end sil function '$s12initializers26callActorMethodFromGeneric1ayAA04SomeC0C_tYaF'

// NI-NS-LABEL: sil hidden [ossa] @$s12initializers26callActorMethodFromGeneric1ayAA04SomeC0C_tYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SomeActor) -> () {
// NI-NS:       bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $SomeActor):
// NI-NS:          hop_to_executor [[ACTOR]]
// NI-NS:          [[FUNC:%.*]] = class_method [[ARG]]
// NI-NS:          apply [[FUNC]]([[ARG]])
// NI-NS-NEXT:     hop_to_executor [[ACTOR]]
// NI-NS: } // end sil function '$s12initializers26callActorMethodFromGeneric1ayAA04SomeC0C_tYaF'
func callActorMethodFromGeneric(a: SomeActor) async {
  await a.someMethod()
}

// NI-LABEL: sil hidden {{.*}} @$s12initializers15makeActorInTaskyyYaF : $@convention(thin) @async () -> () {
// NI:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:          apply
// NI: } // end sil function '$s12initializers15makeActorInTaskyyYaF'

// NI-NS-LABEL: sil hidden [available 10.15] [ossa] @$s12initializers15makeActorInTaskyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// NI-NS:       bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// NI-NS:          hop_to_executor [[ACTOR]]
// NI-NS:          apply
// NI-NS: } // end sil function '$s12initializers15makeActorInTaskyyYaF'

// NI-LABEL: sil private [ossa] @$s12initializers15makeActorInTaskyyYaFAA04SomeC0CyYacfU_ : $@convention(thin) @async @substituted <τ_0_0> (@guaranteed Optional<any Actor>) -> @out τ_0_0 for <SomeActor> {
// NI:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:          apply
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI: } // end sil function '$s12initializers15makeActorInTaskyyYaFAA04SomeC0CyYacfU_'

// NI-NS-LABEL: sil private [ossa] @$s12initializers15makeActorInTaskyyYaFAA04SomeC0CyYacfU_ : $@convention(thin) @async @substituted <τ_0_0> (@guaranteed Optional<any Actor>) -> @out τ_0_0 for <SomeActor> {
// NI-NS:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NS-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI-NS:          apply
// NI-NS-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI-NS: } // end sil function '$s12initializers15makeActorInTaskyyYaFAA04SomeC0CyYacfU_'
@available(SwiftStdlib 5.1, *)
func makeActorInTask() async {
  Task.detached { await SomeActor() }
}

// NI-LABEL: sil hidden {{.*}}  @$s12initializers21callActorMethodInTask1ayAA04SomeC0C_tYaF : $@convention(thin) @async (@guaranteed SomeActor) -> () {
// NI:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// NI-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// NI:          apply
// NI: } // end sil function '$s12initializers21callActorMethodInTask1ayAA04SomeC0C_tYaF'

// NI-NS-LABEL: sil hidden [available 10.15] [ossa] @$s12initializers21callActorMethodInTask1ayAA04SomeC0C_tYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SomeActor) -> () {
// NI-NS: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $SomeActor):
// NI-NS:   hop_to_executor [[ACTOR]]
// NI-NS: } // end sil function '$s12initializers21callActorMethodInTask1ayAA04SomeC0C_tYaF'

// CHECK-LABEL: sil private [ossa] @$s12initializers21callActorMethodInTask1ayAA04SomeC0C_tYaFyyYacfU_ : $@convention(thin) @async @substituted <τ_0_0> (@guaranteed Optional<any Actor>, @guaranteed SomeActor) -> @out τ_0_0 for <()> {
// CHECK:          [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
// CHECK:          apply
// CHECK-NEXT:     hop_to_executor [[GENERIC_EXEC]] : $Optional<Builtin.Executor>
@available(SwiftStdlib 5.1, *)
func callActorMethodInTask(a: SomeActor) async {
  Task.detached { await a.someMethod() }
}
