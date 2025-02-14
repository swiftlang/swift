// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

@resultBuilder
struct TupleBuilder {
  static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
  }

  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildOptional<T>(_ value: T?) -> T? { return value }
}

struct MyTupleStruct<T, U> {
  @TupleBuilder let first: () -> T
  @TupleBuilder let second: U
  // CHECK: init(@TupleBuilder first: @escaping () -> T, @TupleBuilder second: () -> U)
}

// CHECK-LABEL: sil hidden [ossa] @$s25result_builder_memberwise13MyTupleStructV5first6secondACyxq_Gxyc_q_yXEtcfC : $@convention(method) <T, U> (@owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>, @guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <U>, @thin MyTupleStruct<T, U>.Type) -> @out MyTupleStruct<T, U> {
// CHECK: bb0([[SELF:%.*]] : $*MyTupleStruct<T, U>, [[FIRST:%.*]] : @owned $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>, [[SECOND:%.*]] : @guaranteed $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <U>, [[META:%.*]] : $@thin MyTupleStruct<T, U>.Type):
// CHECK-NEXT:   [[FIRST_ADDR:%.*]] = struct_element_addr [[SELF]] : $*MyTupleStruct<T, U>, #MyTupleStruct.first
// CHECK-NEXT:   store [[FIRST]] to [init] [[FIRST_ADDR]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T> 
// CHECK-NEXT:   [[SECOND_ADDR:%.*]] = struct_element_addr [[SELF]] : $*MyTupleStruct<T, U>, #MyTupleStruct.second
// CHECK-NEXT:   [[CALL_RESULT:%.*]] = alloc_stack $U
// CHECK-NEXT:   apply [[SECOND]]([[CALL_RESULT]]) : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <U>
// CHECK-NEXT:   copy_addr [take] [[CALL_RESULT]] to [init] [[SECOND_ADDR]] : $*U
func trigger(cond: Bool) {
  _ = MyTupleStruct {
    1
    "hello"
    if cond {
      "conditional"
    }
  } second: {
    3.14159
    "blah"
  }
}
