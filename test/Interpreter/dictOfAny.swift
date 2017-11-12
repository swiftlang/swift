// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

var dict: [String: Any] = ["foo": 0x11111111 as NSNumber]
print(dict)  // succeeds
// CHECK: ["foo": 286331153]
dict["foo"] = 0x22222222
print(dict)  // used to crash
// CHECK: ["foo": 572662306]
