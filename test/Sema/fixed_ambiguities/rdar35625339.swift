// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s
let arr = Array(10...20)
// CHECK: function_ref @$sSlsE6prefixy11SubSequenceQzSiF
arr.prefix(3).forEach { (v: Int) in }
