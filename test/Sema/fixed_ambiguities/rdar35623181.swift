
// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

extension Sequence where Element == String {
  func record() -> String {
    // CHECK: function_ref @$Ss20LazySequenceProtocolPsE3mapys0a3MapB0Vy8ElementsQzqd__Gqd__7ElementQzclF : $@convention(method) <τ_0_0 where τ_0_0 : LazySequenceProtocol><τ_1_0> (@guaranteed @callee_guaranteed (@in_guaranteed τ_0_0.Element) -> @out τ_1_0, @in_guaranteed τ_0_0) -> @out LazyMapSequence<τ_0_0.Elements, τ_1_0
    return lazy.map({ $0 }).joined(separator: ",")
  }
}
