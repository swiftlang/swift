// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

class S {}
let arr: [(S, Int)] = []
let _: [S] = arr.sorted {
    $0.1 < $1.1
// CHECK: function_ref @$SSlsE6prefixy11SubSequenceQzSiF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0.SubSequence
}.prefix(1).map {
    return $0.0
}
