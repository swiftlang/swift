// RUN: %target-swift-frontend -module-name moveonly_class_inits_end_to_end %s -emit-sil -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -O -emit-sil -sil-verify-all > /dev/null

// A test that makes sure end to end in a copyable class containing a
// non-copyable type, in the init, we only have a single destroy_addr.

// REQUIRES: swift_in_compiler

public struct MO: ~Copyable {
  var x: Int8 = 0
  deinit { print("destroyed MO") }
}

public class MOHaver {
  var mo: MO

  // CHECK-LABEL: sil hidden @$s028moveonly_class_inits_end_to_D07MOHaverCACycfc : $@convention(method) (@owned MOHaver) -> @owned MOHaver {
  // CHECK: bb0([[ARG:%.*]] :
  // CHECK-NEXT: debug_value [[ARG]]
  // CHECK-NEXT: [[EI:%.*]] = end_init_let_ref [[ARG]]
  // CHECK-NEXT: [[META:%.*]] = metatype
  // CHECK-NEXT: function_ref MO.init()
  // CHECK-NEXT: [[FUNC:%.*]] = function_ref @$s028moveonly_class_inits_end_to_D02MOVACycfC :
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[FUNC]]([[META]])
  // CHECK-NEXT: [[REF:%.*]] = ref_element_addr [[EI]]
  // CHECK-NEXT: [[REF_ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF]]
  // CHECK-NEXT: store [[RESULT]] to [[REF_ACCESS]]
  // CHECK-NEXT: end_access [[REF_ACCESS]]
  // CHECK-NEXT: return [[EI]]
  // CHECK-NEXT: } // end sil function '$s028moveonly_class_inits_end_to_D07MOHaverCACycfc'
  init() {
    self.mo = MO()
  }
}
