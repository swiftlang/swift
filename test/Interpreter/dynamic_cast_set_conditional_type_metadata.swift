// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

// rdar://problem/28022201 exposed an ABI mismatch bug between the C++ code
// in the runtime and standard library hook functions written in Swift, which
// led to dynamic cast operations on sets and dictionaries generating corrupt
// type metadata. Exercising this bug requires that the first instantiation of
// a specific dictionary type in the process be through a dynamic cast. We
// then bridge to ObjC, so that the resulting NSDictionary subclass is forced
// to recover the underlying Dictionary's generic environment from the
// corrupted class metadata instead of getting it passed in from the compiler.

import Foundation

let a: Set<String> = ["foo"]
let b: Any = a
let c = b as? Set<AnyHashable>

let d = (c as AnyObject) as! NSSet

_ = d.member("foo")

// CHECK: ok
print("ok")
