// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import Foundation

let opaqueNil: COpaquePointer = nil
if opaqueNil == nil {
  print("ok opaqueNil == nil")
  // CHECK: ok opaqueNil == nil
}

if opaqueNil != nil {
} else {
  print("ok opaqueNil != nil is false")
  // CHECK: ok opaqueNil != nil is false
}

let unsafeNil: UnsafeMutablePointer<Int> = nil
if unsafeNil == (nil as UnsafeMutablePointer<Int>) {
  print("ok unsafeNil == (nil as UnsafeMutablePointer<Int>)")
  // CHECK: ok unsafeNil == (nil as UnsafeMutablePointer<Int>)
}

do {
  try NSFileManager.defaultManager().removeItemAtURL(NSURL(string:"/this/file/does/not/exist")!)
} catch {
  print("ok !removed")
  // CHECK: ok !removed
}

var selNil: Selector = nil
if selNil == nil { 
  print("ok selNil == nil")
  // CHECK: ok selNil == nil
}
selNil = nil
if selNil == nil { 
  print("ok selNil == nil")
  // CHECK: ok selNil == nil
}

