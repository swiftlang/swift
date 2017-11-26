// RUN: %target-swift-frontend -emit-sil -verify %s -swift-version 4 | %FileCheck %s

let x: String = "ultimate question"
// CHECK: function_ref @_T0s26RangeReplaceableCollectionPsE6filterxSb7ElementQzKcKF : $@convention(method) <τ_0_0 where τ_0_0 : RangeReplaceableCollection> (@owned @noescape @callee_guaranteed (@in τ_0_0.Element) -> (Bool, @error Error), @in_guaranteed τ_0_0) -> (@out τ_0_0, @error Error)
_ = x.filter({ $0 == " " }).count < 3
