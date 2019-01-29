// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// REQUIRES: objc_interop

import Foundation

let opaqueNil: OpaquePointer? = nil
if opaqueNil == nil {
  print("ok opaqueNil == nil")
  // CHECK: ok opaqueNil == nil
}

if opaqueNil != nil {
} else {
  print("ok opaqueNil != nil is false")
  // CHECK: ok opaqueNil != nil is false
}

let unsafeNil: UnsafeMutablePointer<Int>? = nil
if unsafeNil == nil {
  print("ok unsafeNil == (nil as UnsafeMutablePointer<Int>)")
  // CHECK: ok unsafeNil == (nil as UnsafeMutablePointer<Int>)
}

do {
  try FileManager.default.removeItem(at: URL(string:"/this/file/does/not/exist")!)
} catch {
  print("ok !removed")
  // CHECK: ok !removed
}

var selNil: Selector? = nil
if selNil == nil { 
  print("ok selNil == nil")
  // CHECK: ok selNil == nil
}
selNil = nil
if selNil == nil { 
  print("ok selNil == nil")
  // CHECK: ok selNil == nil
}

