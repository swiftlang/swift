
// RUN: %target-swift-frontend -emit-sil -verify %s -swift-version 4 | %FileCheck %s

let arr = ["A", "B", "C"]
let lazy: LazyMapCollection = arr.lazy.map { $0 }
// CHECK: function_ref @$ss20LazySequenceProtocolPsE6filterys0a6FilterB0Vy8ElementsQzGSb7ElementQzcF
_ = lazy.filter { $0 > "A" }.count
