// RUN: %target-swift-frontend -enable-experimental-feature InitAccessors -Xllvm -sil-print-after=definite-init -emit-sil %s -o /dev/null 2>&1 | %FileCheck %s

struct Test1 {
  var _a: Int
  var _b: String

  var a: Int {
    init(initialValue) initializes(_a) {
      _a = initialValue
    }

    get { _a }
    set { }
  }

  var b: String {
    init(initialValue) initializes(_a, _b) {
      _a = 0
      _b = initialValue
    }

    get { _b }
    set { }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4null5Test1V1aACSi_tcfC : $@convention(method) (Int, @thin Test1.Type) -> @owned Test1
  // CHECK: assign_or_init [init] {{.*}} : $Int, init {{.*}} : $@convention(thin) (Int) -> @out Int, set {{.*}} : $@callee_guaranteed (Int) -> ()
  // CHECK: assign_or_init [init] {{.*}} : $String, init {{.*}} : $@convention(thin) (@owned String) -> (@out Int, @out String), set {{.*}} : $@callee_guaranteed (@owned String) -> ()
  // CHECK: assign_or_init [set] {{.*}} : $Int, init {{.*}} : $@convention(thin) (Int) -> @out Int, set {{.*}} : $@callee_guaranteed (Int) -> ()
  init(a: Int) {
    self.a = a
    self.a = -1
    self.b = ""
    self.a = a
  }
}
