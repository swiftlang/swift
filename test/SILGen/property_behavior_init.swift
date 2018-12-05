// RUN: %target-swift-frontend -enable-experimental-property-behaviors -emit-silgen -enable-sil-ownership %s | %FileCheck %s
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

  // CHECK-LABEL: sil hidden @$s22property_behavior_init3FooV{{[_0-9a-zA-Z]*}}fC
  // CHECK:       bb0([[X:%.*]] : $Int,
  init(x: Int) {
    // CHECK: [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [rootself]
    // CHECK: [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
    // CHECK: [[UNINIT_STORAGE:%.*]] = struct_element_addr [[PB_BOX]]
    // CHECK: [[UNINIT_BEHAVIOR:%.*]] = mark_uninitialized_behavior {{.*}}<Foo>([[UNINIT_STORAGE]]) : {{.*}}, {{%.*}}([[PB_BOX]])

    // Pure assignments undergo DI analysis so assign to the marking proxy.

    // CHECK: assign [[X]] to [[UNINIT_BEHAVIOR]]
    self.x = x
    // CHECK: assign [[X]] to [[UNINIT_BEHAVIOR]]
    self.x = x

    // Reads or inouts use the accessors normally.

    // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB_BOX]]
    // CHECK: [[SELF:%.*]] = load [trivial] [[READ]]
    // CHECK: [[GETTER:%.*]] = function_ref @$s22property_behavior_init3FooV1xSivg
    // CHECK: apply [[GETTER]]([[SELF]])
    _ = self.x

    // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB_BOX]]
    // CHECK: [[INOUT:%.*]] = alloc_stack
    // CHECK: [[SELF:%.*]] = load [trivial] [[WRITE]]
    // CHECK: [[GETTER:%.*]] = function_ref @$s22property_behavior_init3FooV1xSivg
    // CHECK: [[VALUE:%.*]] = apply [[GETTER]]([[SELF]])
    // CHECK: store [[VALUE]] to [trivial] [[INOUT]]
    // CHECK: [[WHACK:%.*]] = function_ref @$s22property_behavior_init5whackyyxzlF
    // CHECK: apply [[WHACK]]<Int>([[INOUT]])
    // CHECK: [[VALUE:%.*]] = load [trivial] [[INOUT]]
    // CHECK: [[SETTER:%.*]] = function_ref @$s22property_behavior_init3FooV1xSivs
    // CHECK: apply [[SETTER]]([[VALUE]], [[WRITE]])
    whack(&self.x)
  }
}
