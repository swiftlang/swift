// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// rdar://problem/36477954
func AnyToNSObject(_ a: Any) {
  if a is NSObject {
    // ok
  } else {
    fatalError("argument is not bridgable to NSObject")
  }
}

let opt: String? = "hello"
AnyToNSObject(opt as Any)

let doubleOpt: String?? = "hello"
AnyToNSObject(doubleOpt as Any)

let iuo: String! = "goodbye"
AnyToNSObject(iuo as Any)

let doubleIUO: String!! = "goodbye"
AnyToNSObject(doubleIUO as Any)

// rdar://problem/36559165
let dict = NSMutableDictionary()
let kSomeKey: String! = "kSomeKey"
dict.setValue(true as Any, forKey: kSomeKey)
// CHECK: value: 1
print("value:", dict[kSomeKey] ?? "nil")

// CHECK: ok
print("ok")
