// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

class S {}
let arr: [(S, Int)] = []
let _: [S] = arr.sorted {
    $0.1 < $1.1
// CHECK: function_ref @$sSlsE6prefixy11SubSequenceQzSiF
}.prefix(1).map {
    return $0.0
}
