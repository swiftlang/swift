// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature CalledAttribute -swift-version 6 %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: concurrency

class NS {}

struct Box: ~Copyable {
  var ns: NS

  consuming func takeValue() -> NS { ns }
}

func sendAgain(_ ns: sending NS) {}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_sending_captures18testSendingCaptureyyAA2NSCnF : $@convention(thin) (@sil_sending @owned NS) -> () {
// CHECK:  [[NS_PROJ:%.*]] = project_box {{.*}} : ${ var NS }, 0
// CHECK:  [[NS_READ:%.*]] = begin_access [read] [unknown] [[NS_PROJ]] : $*NS
// CHECK:  [[NS_VALUE:%.*]] = load [copy] [[NS_READ]] : $*NS
// CHECK:  [[CLOSURE_REF:%.*]] = function_ref @$s28called_once_sending_captures18testSendingCaptureyyAA2NSCnFyyXOfU_ : $@convention(thin) (@sil_sending @guaranteed NS) -> ()
// CHECK:  [[NS_COPY:%.*]] = copy_value {{%.*}} : $NS
// CHECK:  partial_apply [called_once] [[CLOSURE_REF]]([[NS_COPY]]) : $@convention(thin) (@sil_sending @guaranteed NS) -> ()
// CHECK: } // end sil function '$s28called_once_sending_captures18testSendingCaptureyyAA2NSCnF'

// The closure's own captured parameter carries `@sil_sending`, exactly as a
// genuine `sending` parameter would - this is what makes it eligible to be
// sent onward via `sendAgain`.
// CHECK-LABEL: sil private [ossa] @$s28called_once_sending_captures18testSendingCaptureyyAA2NSCnFyyXOfU_ : $@convention(thin) (@sil_sending @guaranteed NS) -> () {
// CHECK: bb0([[NS_CAPTURE:%.*]] : @closureCapture @guaranteed $NS):
// CHECK:  [[NS_COPY:%.*]] = copy_value [[NS_CAPTURE]] : $NS
// CHECK:  // function_ref sendAgain(_:)
// CHECK:  [[SEND_AGAIN:%.*]] = function_ref @$s28called_once_sending_captures9sendAgainyyAA2NSCnF : $@convention(thin) (@sil_sending @owned NS) -> ()
// CHECK:  apply [[SEND_AGAIN]]([[NS_COPY]]) : $@convention(thin) (@sil_sending @owned NS) -> ()
// CHECK: } // end sil function '$s28called_once_sending_captures18testSendingCaptureyyAA2NSCnFyyXOfU_'
func testSendingCapture(_ ns: sending NS) {
  let g = { @called(once) [sending ns] in sendAgain(ns) }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_sending_captures21testSendingVarCaptureyyAA2NSCnF : $@convention(thin) (@sil_sending @owned NS) -> () {
// CHECK:  copy_addr {{%.*}} to [init] [[NS_PROJ:%.*]] : $*NS
// CHECK:  [[NS_READ:%.*]] = begin_access [read] [unknown] [[NS_PROJ]] : $*NS
// CHECK:  [[NS_VALUE:%.*]] = load [copy] [[NS_READ]] : $*NS
// CHECK:  [[CLOSURE_REF:%.*]] = function_ref @$s28called_once_sending_captures21testSendingVarCaptureyyAA2NSCnFyyXOfU_ : $@convention(thin) (@sil_sending @guaranteed NS) -> ()
// CHECK:  [[NS_COPY:%.*]] = copy_value {{%.*}} : $NS
// CHECK:  partial_apply [called_once] [[CLOSURE_REF]]([[NS_COPY]]) : $@convention(thin) (@sil_sending @guaranteed NS) -> ()
// CHECK: } // end sil function '$s28called_once_sending_captures21testSendingVarCaptureyyAA2NSCnF'
func testSendingVarCapture(_ ns: sending NS) {
  var ns = ns
  let g = { @called(once) [sending ns] in sendAgain(ns) }
  g()
  ns = NS()
}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_sending_captures23testConsumingAndSending3boxyAA3BoxVn_tF : $@convention(thin) (@sil_sending @owned Box) -> () {
// CHECK:  [[BOX_PROJ:%.*]] = project_box {{.*}} : ${ let Box }, 0
// CHECK:  [[CLOSURE_REF:%.*]] = function_ref @$s28called_once_sending_captures23testConsumingAndSending3boxyAA3BoxVn_tFyyXEfU_ : $@convention(thin) (@sil_sending @owned Box) -> ()
// CHECK:  [[BOX_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[BOX_PROJ]] : $*Box
// CHECK:  [[BOX_VALUE:%.*]] = load [take] [[BOX_TAKE_ADDR]] : $*Box
// CHECK:  partial_apply [called_once] [[CLOSURE_REF]]([[BOX_VALUE]]) : $@convention(thin) (@sil_sending @owned Box) -> ()
// CHECK: } // end sil function '$s28called_once_sending_captures23testConsumingAndSending3boxyAA3BoxVn_tF'

// The closure body sees an `@sil_sending @owned` capture, just like a
// genuine `sending` parameter of noncopyable type would.
// CHECK-LABEL: sil private [ossa] @$s28called_once_sending_captures23testConsumingAndSending3boxyAA3BoxVn_tFyyXEfU_ : $@convention(thin) (@sil_sending @owned Box) -> () {
// CHECK: bb0([[BOX_CAPTURE:%.*]] : @closureCapture @owned $Box):
// CHECK:  [[BOX_STACK:%.*]] = alloc_stack $Box, let, name "box"
// CHECK:  [[BOX_INIT_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[BOX_STACK]] : $*Box
// CHECK:  store [[BOX_CAPTURE]] to [init] [[BOX_INIT_ADDR]] : $*Box
// CHECK:  [[BOX_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[BOX_INIT_ADDR]] : $*Box
// CHECK:  [[BOX_VALUE:%.*]] = load [copy] [[BOX_ADDR]] : $*Box
// CHECK:  // function_ref Box.takeValue()
// CHECK:  [[TAKE_VALUE:%.*]] = function_ref @$s28called_once_sending_captures3BoxV9takeValueAA2NSCyF : $@convention(method) (@owned Box) -> @owned NS
// CHECK:  [[NS_VALUE:%.*]] = apply [[TAKE_VALUE]]([[BOX_VALUE]]) : $@convention(method) (@owned Box) -> @owned NS
// CHECK:  // function_ref sendAgain(_:)
// CHECK:  [[SEND_AGAIN:%.*]] = function_ref @$s28called_once_sending_captures9sendAgainyyAA2NSCnF : $@convention(thin) (@sil_sending @owned NS) -> ()
// CHECK:  apply [[SEND_AGAIN]]({{%.*}}) : $@convention(thin) (@sil_sending @owned NS) -> ()
// CHECK: } // end sil function '$s28called_once_sending_captures23testConsumingAndSending3boxyAA3BoxVn_tFyyXEfU_'
func testConsumingAndSending(box: consuming sending Box) {
  func calledOnce(_: @called(once) () -> Void) {}

  calledOnce { [sending box] in
    let ns = box.takeValue()
    sendAgain(ns)
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_sending_captures22testCapturePropagationyyAA2NSCnF : $@convention(thin) (@sil_sending @owned NS) -> () {
// CHECK:  [[NS_PROJ:%.*]] = project_box {{.*}} : ${ var NS }, 0
// CHECK:  [[NS_READ:%.*]] = begin_access [read] [unknown] [[NS_PROJ]] : $*NS
// CHECK:  [[NS_VALUE:%.*]] = load [copy] [[NS_READ]] : $*NS
// CHECK:  [[OUTER_CLOSURE_REF:%.*]] = function_ref @$s28called_once_sending_captures22testCapturePropagationyyAA2NSCnFyyXOfU_ : $@convention(thin) (@sil_sending @guaranteed NS) -> ()
// CHECK:  [[NS_COPY:%.*]] = copy_value {{%.*}} : $NS
// CHECK:  partial_apply [called_once] [[OUTER_CLOSURE_REF]]([[NS_COPY]]) : $@convention(thin) (@sil_sending @guaranteed NS) -> ()
// CHECK: } // end sil function '$s28called_once_sending_captures22testCapturePropagationyyAA2NSCnF'

// CHECK-LABEL: sil private [ossa] @$s28called_once_sending_captures22testCapturePropagationyyAA2NSCnFyyXOfU_ : $@convention(thin) (@sil_sending @guaranteed NS) -> () {
// CHECK: bb0([[NS_OUTER_CAPTURE:%.*]] : @closureCapture @guaranteed $NS):
// CHECK:  [[INNER_CLOSURE_REF:%.*]] = function_ref @$s28called_once_sending_captures22testCapturePropagationyyAA2NSCnFyyXOfU_yyXEfU_ : $@convention(thin) (@sil_sending @guaranteed NS) -> ()
// CHECK:  [[NS_COPY:%.*]] = copy_value [[NS_OUTER_CAPTURE]] : $NS
// CHECK:  partial_apply [called_once] [[INNER_CLOSURE_REF]]([[NS_COPY]]) : $@convention(thin) (@sil_sending @guaranteed NS) -> ()
// CHECK: } // end sil function '$s28called_once_sending_captures22testCapturePropagationyyAA2NSCnFyyXOfU_'

// CHECK-LABEL: sil private [ossa] @$s28called_once_sending_captures22testCapturePropagationyyAA2NSCnFyyXOfU_yyXEfU_ : $@convention(thin) (@sil_sending @guaranteed NS) -> () {
// CHECK: bb0([[NS_INNER_CAPTURE:%.*]] : @closureCapture @guaranteed $NS):
// CHECK:  [[NS_COPY:%.*]] = copy_value [[NS_INNER_CAPTURE]] : $NS
// CHECK:  // function_ref sendAgain(_:)
// CHECK:  [[SEND_AGAIN:%.*]] = function_ref @$s28called_once_sending_captures9sendAgainyyAA2NSCnF : $@convention(thin) (@sil_sending @owned NS) -> ()
// CHECK:  apply [[SEND_AGAIN]]([[NS_COPY]]) : $@convention(thin) (@sil_sending @owned NS) -> ()
// CHECK: } // end sil function '$s28called_once_sending_captures22testCapturePropagationyyAA2NSCnFyyXOfU_yyXEfU_'
func testCapturePropagation(_ ns: sending NS) {
  func calledOnce(_: @called(once) () -> Void) {}

  let _: @called(once) () -> Void = { [sending ns] in
    calledOnce {
      sendAgain(ns)
    }
  }
}
