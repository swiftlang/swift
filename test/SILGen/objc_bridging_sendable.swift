// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/objc_bridging_sendable.h %s | %FileCheck %s

// REQUIRES: objc_interop

// CHECK-LABEL: sil [ossa] @$s22objc_bridging_sendable18passSendableToObjCyys0E0_pF : $@convention(thin) (@in_guaranteed any Sendable) -> () {
// CHECK:  function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned AnyObject
// CHECK: objc_method {{%.*}} : $NSBlah, #NSBlah.takeSendable!foreign : (NSBlah) -> ((any Sendable)?) -> (), $@convention(objc_method) (Optional<AnyObject>, NSBlah) -> ()
// CHECK: return
public func passSendableToObjC(_ s: Sendable) {
  NSBlah().takeSendable(s)
}