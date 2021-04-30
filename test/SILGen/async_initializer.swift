// RUN: %target-swift-frontend -emit-silgen %s -module-name initializers -swift-version 5 -enable-experimental-concurrency | %FileCheck --enable-var-scope %s
// REQUIRES: concurrency

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
// CHECK-NEXT:    required init() async
// CHECK-NEXT:    init(name: String) async

// CHECK:       @_inheritsConvenienceInitializers class NorthAmericaPerson : EarthPerson {
// CHECK-NEXT:    required init() async
// CHECK-NEXT:    override init(name: String) async

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
  // CHECK-DAG: sil hidden [ossa] @$s12initializers8MyStructVACyYacfC : $@convention(method) @async (@thin MyStruct.Type) -> MyStruct
  init() async {}
}

enum MyEnum {
  // CHECK-DAG: sil hidden [ossa] @$s12initializers6MyEnumOACyYacfC : $@convention(method) @async (@thin MyEnum.Type) -> MyEnum
  init() async {}
}

actor MyActor {
  // CHECK-DAG:   sil hidden [ossa] @$s12initializers7MyActorCACyYacfc : $@convention(method) @async (@owned MyActor) -> @owned MyActor
  // CHECK-NOT:     hop_to_executor
  // CHECK-DAG:   } // end sil function '$s12initializers7MyActorCACyYacfc'
  init() async {}
}

class EarthPerson : Person {
  // CHECK-DAG: sil hidden [ossa] @$s12initializers11EarthPersonCACyYacfc : $@convention(method) @async (@owned EarthPerson) -> @owned EarthPerson
  required init() async {}

  // CHECK-DAG: sil hidden [ossa] @$s12initializers11EarthPersonC4nameACSS_tYacfc : $@convention(method) @async (@owned String, @owned EarthPerson) -> @owned EarthPerson
  init(name: String) async {}

  // CHECK-DAG: sil private [transparent] [thunk] [ossa] @$s12initializers11EarthPersonCAA0C0A2aDPxyYacfCTW : $@convention(witness_method: Person) @async (@thick EarthPerson.Type) -> @out EarthPerson
}

class NorthAmericaPerson : EarthPerson {
  // CHECK-DAG: sil hidden [ossa] @$s12initializers18NorthAmericaPersonCACyYacfc : $@convention(method) @async (@owned NorthAmericaPerson) -> @owned NorthAmericaPerson
  required init() async { await super.init() }

  // CHECK-DAG: sil hidden [ossa] @$s12initializers18NorthAmericaPersonC4nameACSS_tYacfc : $@convention(method) @async (@owned String, @owned NorthAmericaPerson) -> @owned NorthAmericaPerson
  override init(name: String) async { await super.init(name: name) }
}

////
// check that of global-actor isolation is preserved in async initializers.
// there's also coverage for sync global-actor-isolated initializers

func someAsyncFn() async {}

class Cat {
  // CHECK-LABEL:  sil hidden [ossa] @$s12initializers3CatCACyYacfc : $@convention(method) @async (@owned Cat) -> @owned Cat {
  // CHECK:          hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:          {{%[0-9]+}} = function_ref @$s12initializers11someAsyncFnyyYaF : $@convention(thin) @async () -> ()
  // CHECK:          {{%[0-9]+}} = apply {{%[0-9]+}}() : $@convention(thin) @async () -> ()
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
  // CHECK:          {{%[0-9]+}} = function_ref @$s12initializers11someAsyncFnyyYaF : $@convention(thin) @async () -> ()
  // CHECK:          {{%[0-9]+}} = apply {{%[0-9]+}}() : $@convention(thin) @async () -> ()
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
  // CHECK:          {{%[0-9]+}} = function_ref @$s12initializers11someAsyncFnyyYaF : $@convention(thin) @async () -> ()
  // CHECK:          {{%[0-9]+}} = apply {{%[0-9]+}}() : $@convention(thin) @async () -> ()
  // CHECK-NEXT:     hop_to_executor {{%[0-9]+}} : $MainActor
  // CHECK:        } // end sil function '$s12initializers4BirbOACyYacfC'
  @MainActor init() async {
    await someAsyncFn()
  }

  @MainActor init(name: String) {}
}

// CHECK-LABEL:  sil hidden [ossa] @$s12initializers7makeCatyyYaF : $@convention(thin) @async () -> () {
// CHECK:          hop_to_executor {{%[0-9]+}} : $MainActor
// CHECK-NEXT:     {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thick Cat.Type) -> @owned Cat
// CHECK-NEXT:     hop_to_executor {{%[0-9]+}} : $Optional<Builtin.Executor>
// CHECK:        } // end sil function '$s12initializers7makeCatyyYaF'
func makeCat() async {
  _ = await Cat(name: "Socks")
}

// CHECK-LABEL:  sil hidden [ossa] @$s12initializers7makeDogyyYaF : $@convention(thin) @async () -> () {
// CHECK:          hop_to_executor {{%[0-9]+}} : $MainActor
// CHECK-NEXT:     {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thin Dog.Type) -> Dog
// CHECK-NEXT:     hop_to_executor {{%[0-9]+}} : $Optional<Builtin.Executor>
// CHECK:        } // end sil function '$s12initializers7makeDogyyYaF'
func makeDog() async {
  _ = await Dog(name: "Lassie")
}

// CHECK-LABEL:  sil hidden [ossa] @$s12initializers8makeBirbyyYaF : $@convention(thin) @async () -> () {
// CHECK:          hop_to_executor {{%[0-9]+}} : $MainActor
// CHECK-NEXT:     {{%[0-9]+}} = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (@owned String, @thin Birb.Type) -> Birb
// CHECK-NEXT:     hop_to_executor {{%[0-9]+}} : $Optional<Builtin.Executor>
// CHECK:        } // end sil function '$s12initializers8makeBirbyyYaF'
func makeBirb() async {
  _ = await Birb(name: "Chirpy")
}