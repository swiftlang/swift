// RUN: %target-swift-frontend -emit-silgen %s -disable-availability-checking | %FileCheck %s

// REQUIRES: concurrency

class NotSendable {}

func callee(_ ns: NotSendable) {}

actor MyActor {
  func isolatedToSelf(ns: NotSendable) {
    // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation7MyActorC14isolatedToSelf2nsyAA11NotSendableC_tF08implicitH7CaptureL_yyYaF : $@convention(thin) @async (@guaranteed NotSendable, @sil_isolated @guaranteed MyActor) -> () {
    func implicitSelfCapture() async {

      // CHECK: [[COPY:%.*]] = copy_value %1 : $MyActor
      // CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[COPY]] : $MyActor
      // CHECK-NEXT: hop_to_executor [[BORROW]] : $MyActor

      // CHECK: [[FN:%.*]] = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
      // CHECK-NEXT: apply [[FN]]() : $@convention(thin) @async () -> ()
      await test()

      // CHECK: hop_to_executor [[BORROW]] : $MyActor
      // CHECK: [[FN:%.*]] = function_ref @$s24local_function_isolation6calleeyyAA11NotSendableCF : $@convention(thin) (@guaranteed NotSendable) -> ()
      // CHECK-NEXT: apply [[FN]](%0) : $@convention(thin) (@guaranteed NotSendable) -> ()

      // we need to hop back to 'self' here
      callee(ns)

      // CHECK: end_borrow [[BORROW]] : $MyActor
      // CHECK-NEXT: destroy_value [[COPY]] : $MyActor
    }
  }
}

func f(isolation: isolated MyActor, ns: NotSendable) {
  // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation1f0C02nsyAA7MyActorCYi_AA11NotSendableCtF23implicitIsolatedCaptureL_yyYaF : $@convention(thin) @async (@guaranteed NotSendable, @sil_isolated @guaranteed MyActor) -> () {
  func implicitIsolatedCapture() async {

    // CHECK: [[COPY:%.*]] = copy_value %1 : $MyActor
    // CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[COPY]] : $MyActor
    // CHECK-NEXT: hop_to_executor [[BORROW]] : $MyActor

    // CHECK: [[FN:%.*]] = function_ref @$s24local_function_isolation4testyyYaF : $@convention(thin) @async () -> ()
    // CHECK-NEXT: apply [[FN]]() : $@convention(thin) @async () -> ()
    await test()

    // we need to hop back to 'isolation' here
    callee(ns)

    // CHECK: end_borrow [[BORROW]] : $MyActor
    // CHECK-NEXT: destroy_value [[COPY]] : $MyActor
  }
}

func test() async {}

// A generic actor type, which causes the generic self parameter
// (actor isolation parameter) to be added to captures of the
// local/nested function.
actor GenericActor<K> {
  var i: Int = 0
  private func outerFunc() {
    func accessSelf() -> Int {
      // CHECK-LABEL: sil private [ossa] @$s24local_function_isolation12GenericActorC9outerFunc33_7B9E2B75110B8600A136A469D51CAF2BLLyyF10accessSelfL_SiylF : $@convention(thin) <K> (@sil_isolated @guaranteed GenericActor<K>) -> Int {
      return 0
    }
    print(accessSelf())
  }
}
