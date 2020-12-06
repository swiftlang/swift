// RUN: %target-swift-frontend -emit-silgen %s -swift-version 5 -enable-experimental-concurrency | %FileCheck -check-prefix CHECK %s
// REQUIRES: concurrency

public protocol DefaultInit {
  init()
}

public actor class A1<T: DefaultInit> {
  var x: Int = 17
  var y: T = T()

  public func f() { }
}

extension Int: DefaultInit { }

public actor class A2 {
  func f() { }
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}

func buildIt() {
  _ = A1<Int>()
}

// A1.enqueue(partialTask:)
// CHECK-LABEL: sil [ossa] @$s29synthesized_conformance_actor2A1C7enqueue11partialTasky12_Concurrency012PartialAsyncG0V_tF : $@convention(method) <T where T : DefaultInit> (PartialAsyncTask, @guaranteed A1<T>) -> () {
// CHECK: bb0([[PARTIAL_TASK:%.*]] : $PartialAsyncTask, [[SELF:%.*]] : @guaranteed $A1<T>):
// CHECK:       [[SELF_COPY:%.*]] = copy_value [[SELF]] : $A1<T>
// CHECK-NEXT:  [[SELF_ANY_OBJECT:%.*]] = init_existential_ref [[SELF_COPY]] : $A1<T> : $A1<T>, $AnyObject
// CHECK:       [[ENQUEUE_FN:%.*]] = function_ref @swift_defaultActor_enqueue : $@convention(thin) (PartialAsyncTask, @guaranteed AnyObject) -> ()
// CHECK-NEXT:  apply [[ENQUEUE_FN]]([[PARTIAL_TASK]], [[SELF_ANY_OBJECT]]) : $@convention(thin) (PartialAsyncTask, @guaranteed AnyObject) -> ()

// Ensure that enqueue(partialTask:) is the first slot in the vtable.
// CHECK-LABEL: sil_vtable [serialized] A1 {
// CHECK-NEXT:    #A1.enqueue: <T where T : DefaultInit> (A1<T>) -> (PartialAsyncTask) -> () : @$s29synthesized_conformance_actor2A1C7enqueue11partialTasky12_Concurrency012PartialAsyncG0V_tF

// CHECK-LABEL: sil_vtable [serialized] A2 {
// CHECK-NEXT:    #A2.enqueue: (A2) -> (PartialAsyncTask) -> () : @$s29synthesized_conformance_actor2A2C7enqueue11partialTasky12_Concurrency012PartialAsyncG0V_tF // A2.enqueue(partialTask:)
