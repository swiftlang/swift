// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -enable-experimental-property-behaviors -emit-silgen %s | %FileCheck %s
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

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = Int

  // TODO
  // var xx: (Int, Int) __behavior diBehavior

  // CHECK-LABEL: sil hidden @_T022property_behavior_init3FooV{{[_0-9a-zA-Z]*}}fC
  // CHECK:       bb0([[X:%.*]] : $Int,
  init(x: Int) {
    // CHECK: [[UNINIT_SELF:%.*]] = mark_uninitialized [rootself]
    // CHECK: [[UNINIT_STORAGE:%.*]] = struct_element_addr [[UNINIT_SELF]]
    // CHECK: [[UNINIT_BEHAVIOR:%.*]] = mark_uninitialized_behavior {{.*}}<Foo>([[UNINIT_STORAGE]]) : {{.*}}, {{%.*}}([[UNINIT_SELF]])

    // Pure assignments undergo DI analysis so assign to the marking proxy.

    // CHECK: assign [[X]] to [[UNINIT_BEHAVIOR]]
    self.x = x
    // CHECK: assign [[X]] to [[UNINIT_BEHAVIOR]]
    self.x = x

    // Reads or inouts use the accessors normally.

    // CHECK: [[SELF:%.*]] = load [trivial] [[UNINIT_SELF]]
    // CHECK: [[GETTER:%.*]] = function_ref @_T022property_behavior_init3FooV1xSifg
    // CHECK: apply [[GETTER]]([[SELF]])
    _ = self.x

    // CHECK: [[WHACK:%.*]] = function_ref @_T022property_behavior_init5whackyxzlF
    // CHECK: [[INOUT:%.*]] = alloc_stack
    // CHECK: [[SELF:%.*]] = load [trivial] [[UNINIT_SELF]]
    // CHECK: [[GETTER:%.*]] = function_ref @_T022property_behavior_init3FooV1xSifg
    // CHECK: [[VALUE:%.*]] = apply [[GETTER]]([[SELF]])
    // CHECK: store [[VALUE]] to [trivial] [[INOUT]]
    // CHECK: apply [[WHACK]]<Int>([[INOUT]])
    // CHECK: [[VALUE:%.*]] = load [trivial] [[INOUT]]
    // CHECK: [[SETTER:%.*]] = function_ref @_T022property_behavior_init3FooV1xSifs
    // CHECK: apply [[SETTER]]([[VALUE]], [[UNINIT_SELF]])
    whack(&self.x)
  }
}
