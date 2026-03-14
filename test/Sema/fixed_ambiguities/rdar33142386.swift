
// RUN: %target-swift-frontend -emit-sil -verify %s -swift-version 4 | %FileCheck %s

let x: String = "ultimate question"
// CHECK: function_ref @$sSmsE6filteryxSb7ElementQzqd__YKXEqd__YKs5ErrorRd__lF
_ = x.filter({ $0 == " " }).count < 3
