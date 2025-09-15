// RUN: %target-swift-frontend %s -Xllvm -sil-print-types -emit-silgen | %FileCheck %s

public struct ID {
  var a: Int = 0
  var b: Int = 1
  var c: Int = 2
  var d: Int = 3
  var e: Int = 4
  var description: String = ""
  var mirror: Mirror
}

struct Test {
  let id: ID
  private var _name: String

  var name: String {
    @storageRestrictions(initializes: _name)
    init { _name = newValue }

    get { _name }

    nonmutating set {}
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4main4TestV2id4nameAcA2IDV_SStcfC : $@convention(method) (@in ID, @owned String, @thin Test.Type) -> @out Test
  // CHECK: [[SELF:%.*]] = project_box {{.*}} : ${ var Test }, 0
  // CHECK: [[SELF_REF:%.*]] = begin_access [read] [unknown] [[SELF]] : $*Test
  // CHECK: [[INIT_REF:%.*]] = function_ref @$s4main4TestV4nameSSvi : $@convention(thin) (@owned String, @thin Test.Type) -> @out String
  // CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thin Test.Type
  // CHECK-NEXT: [[INIT:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[INIT_REF]]([[METATYPE]]) : $@convention(thin) (@owned String, @thin Test.Type) -> @out String
  // CHECK: [[SETTER_REF:%.*]] = function_ref @$s4main4TestV4nameSSvs : $@convention(method) (@owned String, @in_guaranteed Test) -> ()
  // CHECK-NEXT: [[SETTER:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[SETTER_REF]]([[SELF_REF]]) : $@convention(method) (@owned String, @in_guaranteed Test) -> ()
  // CHECK-NEXT: assign_or_init #Test.name, self [[SELF]] : $*Test, value {{.*}} : $String, init [[INIT]] : $@noescape @callee_guaranteed (@owned String) -> @out String, set [[SETTER]] : $@noescape @callee_guaranteed (@owned String) -> ()
  init(id: ID, name: String) {
    self.id = id
    self.name = name
  }
}
