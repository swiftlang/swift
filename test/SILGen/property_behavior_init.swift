// RUN: %target-swift-frontend -enable-experimental-property-behaviors -emit-silgen %s | %FileCheck %s
protocol diBehavior {
  associatedtype Value
  var storage: Value { get set }
}
extension diBehavior {
  var value: Value {
    get { }
    set { }
  }

  static func initStorage(_ initial: Value) -> Value { }
}

func whack<T>(_ x: inout T) {}

struct Foo {
  var x: Int __behavior diBehavior

  // TODO
  // var xx: (Int, Int) __behavior diBehavior

  // CHECK-LABEL: sil hidden @_TFV22property_behavior_init3FooC
  // CHECK:       bb0([[X:%.*]] : $Int,
  init(x: Int) {
    // CHECK: [[UNINIT_SELF:%.*]] = mark_uninitialized [rootself]
    // CHECK: [[UNINIT_STORAGE:%.*]] = struct_element_addr [[UNINIT_SELF]]
    // CHECK: [[UNINIT_BEHAVIOR:%.*]] = mark_uninitialized_behavior {{.*}}<Foo, Int>([[UNINIT_STORAGE]]) : {{.*}}, {{%.*}}([[UNINIT_SELF]])

    // Pure assignments undergo DI analysis so assign to the marking proxy.

    // CHECK: assign [[X]] to [[UNINIT_BEHAVIOR]]
    self.x = x
    // CHECK: assign [[X]] to [[UNINIT_BEHAVIOR]]
    self.x = x

    // Reads or inouts use the accessors normally.

    // CHECK: [[SELF:%.*]] = load [[UNINIT_SELF]]
    // CHECK: [[GETTER:%.*]] = function_ref @_TFV22property_behavior_init3Foog1xSi
    // CHECK: apply [[GETTER]]([[SELF]])
    _ = self.x

    // CHECK: [[WHACK:%.*]] = function_ref @_TF22property_behavior_init5whackurFRxT_
    // CHECK: [[INOUT:%.*]] = alloc_stack
    // CHECK: [[SELF:%.*]] = load [[UNINIT_SELF]]
    // CHECK: [[GETTER:%.*]] = function_ref @_TFV22property_behavior_init3Foog1xSi
    // CHECK: [[VALUE:%.*]] = apply [[GETTER]]([[SELF]])
    // CHECK: store [[VALUE]] to [[INOUT]]
    // CHECK: apply [[WHACK]]<Int>([[INOUT]])
    // CHECK: [[VALUE:%.*]] = load [[INOUT]]
    // CHECK: [[SETTER:%.*]] = function_ref @_TFV22property_behavior_init3Foos1xSi
    // CHECK: apply [[SETTER]]([[VALUE]], [[UNINIT_SELF]])
    whack(&self.x)
  }
}
