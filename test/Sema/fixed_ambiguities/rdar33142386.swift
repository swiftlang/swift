
// RUN: %target-swift-frontend -emit-sil -verify %s -swift-version 4 | %FileCheck %s

let x: String = "ultimate question"
// CHECK: function_ref @$sSmsE6filteryxSb7ElementQzKXEKF
_ = x.filter({ $0 == " " }).count < 3
