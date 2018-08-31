// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s
let arr = Array(10...20)
// CHECK: function_ref @$SSlsE6prefixy11SubSequenceQzSiF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0.SubSequence
arr.prefix(3).forEach { (v: Int) in }
