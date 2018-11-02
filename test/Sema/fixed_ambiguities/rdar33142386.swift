
// RUN: %target-swift-frontend -emit-sil -verify %s -swift-version 4 | %FileCheck %s

let x: String = "ultimate question"
// CHECK: function_ref @$SSmsE6filteryxSb7ElementQzKXEKF : $@convention(method) <τ_0_0 where τ_0_0 : RangeReplaceableCollection> (@noescape @callee_guaranteed (@in_guaranteed τ_0_0.Element) -> (Bool, @error Error), @in_guaranteed τ_0_0) -> (@out τ_0_0, @error Error)
_ = x.filter({ $0 == " " }).count < 3
