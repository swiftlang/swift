// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s  -target %target-swift-5.1-abi-triple | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s  -target %target-swift-5.1-abi-triple -strict-concurrency=targeted | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s  -target %target-swift-5.1-abi-triple -strict-concurrency=complete | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s  -target %target-swift-5.1-abi-triple -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_RegionBasedIsolation

actor A {
  var x: String = "Hello"
  var y: String = "World"
}
// CHECK-LABEL: sil hidden @$s21default_actor_definit1ACACycfc
// CHECK:         builtin "initializeDefaultActor"(%0 : $A) : $()
// CHECK-LABEL: end sil function '$s21default_actor_definit1ACACycfc'

actor B {
  var x: String
  var y: String

  init() {
    x = "Hello"
    y = "World"
  }
}
// CHECK-LABEL: sil hidden @$s21default_actor_definit1BCACycfc
// CHECK:         builtin "initializeDefaultActor"(%0 : $B) : $()
// CHECK-LABEL: end sil function '$s21default_actor_definit1BCACycfc'

actor C {
  var x: String
  var y: Int

  init?(y: Int) {
    self.x = "Hello"
    guard y >= 0 else { return nil }
    self.y = y
  }

  convenience init?(yy: Int) {
    guard yy <= 100 else { return nil }
    self.init(y: yy)
  }
}
// CHECK-LABEL: sil hidden @$s21default_actor_definit1CC1yACSgSi_tcfc
// CHECK:         builtin "initializeDefaultActor"(%1 : $C)
// CHECK:         [[EI:%.*]] = end_init_let_ref %1
// CHECK:         [[X:%.*]] = ref_element_addr [[EI]] : $C, #C.x
// CHECK-NEXT:    [[ACCESS:%.*]] = begin_access [init] [static] [[X]] : $*String
// CHECK-NEXT:    store {{.*}} to [[ACCESS]] : $*String
// CHECK-NEXT:    end_access [[ACCESS]] : $*String
// CHECK:         cond_br {{%.*}}, bb1, bb2
// CHECK:       bb1:
// CHECK:         ref_element_addr [[EI]] : $C, #C.y
// CHECK:         br bb3
// CHECK:       bb2:
// CHECK:         [[X:%.*]] = ref_element_addr [[EI]] : $C, #C.x
// CHECK-NEXT:    [[ACCESS:%.*]] = begin_access [deinit] [static] [[X]] : $*String
// CHECK-NEXT:    destroy_addr [[ACCESS]] : $*String
// CHECK-NEXT:    end_access [[ACCESS]] : $*String
// CHECK:         builtin "destroyDefaultActor"([[EI]] : $C)
// CHECK:         br bb3
// CHECK-LABEL: end sil function '$s21default_actor_definit1CC1yACSgSi_tcfc'

// CHECK-LABEL: sil hidden @$s21default_actor_definit1CC2yyACSgSi_tcfC
// CHECK-NOT:     builtin "initializeDefaultActor"
// CHECK-NOT:     builtin "destroyDefaultActor"
// CHECK-LABEL: end sil function '$s21default_actor_definit1CC2yyACSgSi_tcfC'
