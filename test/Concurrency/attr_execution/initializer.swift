// RUN: %target-swift-frontend -enable-experimental-concurrency -target %target-swift-5.1-abi-triple -emit-silgen -o - -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault %s | %FileCheck --implicit-check-not=hop_to_executor %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// This file contains tests specifically for async initializers that do not
// include type checker errors so we can test the SIL part of the pipeline. Put type checker errors into
// async_initializer.swift.

// CHECK-LABEL: sil hidden [ossa] @$s11initializer1fyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// CHECK:  hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s11initializer1fyyYaF'
func f() async {}
func g() {}

class Fruit {
  // Fruit.__allocating_init()
  // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil hidden [exact_self_class] [ossa] @$s11initializer5FruitCACyYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thick Fruit.Type) -> @owned Fruit {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s11initializer5FruitCACyYacfC'

  // Fruit.init()
  // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil hidden [ossa] @$s11initializer5FruitCACyYacfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @owned Fruit) -> @owned Fruit {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $Optional<any Actor>,
  // CHECK:   hop_to_executor [[ARG]]
  // CHECK: } // end sil function '$s11initializer5FruitCACyYacfc'
  init() async {}

  // CHECK-LABEL: sil hidden [exact_self_class] [ossa] @$s11initializer5FruitC4nameACSS_tcfC : $@convention(method) (@owned String, @thick Fruit.Type) -> @owned Fruit {
  // CHECK: } // end sil function '$s11initializer5FruitC4nameACSS_tcfC'

  // CHECK-LABEL: sil hidden [ossa] @$s11initializer5FruitC4nameACSS_tcfc : $@convention(method) (@owned String, @owned Fruit) -> @owned Fruit {
  // CHECK: } // end sil function '$s11initializer5FruitC4nameACSS_tcfc'
  init(name: String) {}
}

class Banana: Fruit {
  // TODO: Make this not async. We currently miscompile if this is sync.
  //
  // class Fruit {
  //   required init() async {}
  //   init(name: String) {}
  // }
  // 
  // class Banana: Fruit {
  //   override required init() {
  //     super.init(name: "banana")
  //   }
  // }
  // 
  // func test(_ x: Fruit.Type) async {
  //   await print(x.init())
  // }
  // 
  // await test(Banana.self)

  // Banana.__allocating_init()
  // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil hidden [exact_self_class] [ossa] @$s11initializer6BananaCACyYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thick Banana.Type) -> @owned Banana {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s11initializer6BananaCACyYacfC'

  // Banana.init()
  // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil hidden [ossa] @$s11initializer6BananaCACyYacfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @owned Banana) -> @owned Banana {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s11initializer6BananaCACyYacfc'
  override init() async {
    super.init(name: "banana")
  }
}

class MyType {
  // MyType.__allocating_init(_:)
  // Isolation: caller_isolation_inheriting
  //
  // CHECK-LABEL: sil hidden [exact_self_class] [ossa] @$s11initializer6MyTypeCyACyyYaYCXEYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> (), @thick MyType.Type) -> @owned MyType {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $@noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s11initializer6MyTypeCyACyyYaYCXEYacfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> (), @owned MyType) -> @owned MyType
  // CHECK:   apply [[FUNC]]([[ACTOR]], [[ARG]], {{%.*}}) :
  // CHECK: } // end sil function '$s11initializer6MyTypeCyACyyYaYCXEYacfC'

  // MyType.init(_:)
  // Isolation: caller_isolation_inheriting
  // CHECK-LABEL: sil hidden [ossa] @$s11initializer6MyTypeCyACyyYaYCXEYacfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> (), @owned MyType) -> @owned MyType {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $@noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> (), [[SELF:%.*]] : @owned $MyType):
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK:   [[ARG_C:%.*]] = copy_value [[ARG]]
  // CHECK:   [[ARG_B:%.*]] = begin_borrow [[ARG_C]]
  // CHECK:   apply [[ARG_B]]([[ACTOR]])
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s11initializer6MyTypeCyACyyYaYCXEYacfc'
  init(_ f: () async -> Void) reasync {
    await f()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s11initializer4beepyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[FUNC:%.*]] = function_ref @$s11initializer6MyTypeCyACyyYaYCXEYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> (), @thick MyType.Type) -> @owned MyType
// CHECK:   apply [[FUNC]]([[ACTOR]], {{%.*}}, {{%.*}})
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s11initializer4beepyyYaF'
func beep() async {
  let _ = await MyType(f)
  let _ = MyType(g)
}

// thunk for @escaping @callee_guaranteed () -> ()
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIeg_ScA_pSgIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @callee_guaranteed () -> ()) -> () {
// CHECK: hop_to_executor
// CHECK: } // end sil function '$sIeg_ScA_pSgIegHgIL_TR'

actor A {
  // CHECK-LABEL: sil hidden [ossa] @$s11initializer1ACACyYacfc : $@convention(method) @async (@sil_isolated @owned A) -> @owned A {
  // CHECK: hop_to_executor
  // CHECK: hop_to_executor
  // CHECK: } // end sil function '$s11initializer1ACACyYacfc'
  init() async {
    await f()
  }

  init() {
  }
}

protocol AsyncDefaultConstructable {
  init() async
}

struct Location {
  var x : Int
  var y : Int
  // CHECK-LABEL: sil hidden [ossa] @$s11initializer8LocationVACyYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thin Location.Type) -> Location {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
  // CHECK: hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s11initializer8LocationVACyYacfC'
  init() async {
    self.x = 0
    self.y = 0
  }
}

extension Location: AsyncDefaultConstructable {}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s11initializer8LocationVAA25AsyncDefaultConstructableA2aDPxyYacfCTW : $@convention(witness_method: AsyncDefaultConstructable) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thick Location.Type) -> @out Location {
// CHECK: bb0([[INDIRECT_RESULT:%.*]] : $*Location, [[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
// CHECK:   [[FUNC:%.*]] = function_ref @$s11initializer8LocationVACyYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thin Location.Type) -> Location
// CHECK:   apply [[FUNC]]([[ACTOR]],
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s11initializer8LocationVAA25AsyncDefaultConstructableA2aDPxyYacfCTW'

// CHECK-LABEL: sil hidden [exact_self_class] [ossa] @$s11initializer12ExplicitTestCACyYaKcfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thick ExplicitTest.Type) -> (@owned ExplicitTest, @error any Error) {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[FUNC:%.*]] = function_ref @$s11initializer12ExplicitTestCACyYaKcfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @owned ExplicitTest) -> (@owned ExplicitTest, @error any Error)
// CHECK:   try_apply [[FUNC]]([[ACTOR]], {{%.*}})
// CHECK: } // end sil function '$s11initializer12ExplicitTestCACyYaKcfC'

// CHECK-LABEL: sil hidden [ossa] @$s11initializer12ExplicitTestCACyYaKcfc : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @owned ExplicitTest) -> (@owned ExplicitTest, @error any Error) {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s11initializer12ExplicitTestCACyYaKcfc'
class ExplicitTest {
    nonisolated(nonsending) init() async throws {
    }
}
