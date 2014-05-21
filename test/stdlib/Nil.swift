// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

let opaqueNil: COpaquePointer = nil
if opaqueNil == nil {
  println("ok opaqueNil == nil")
  // CHECK: ok opaqueNil == nil
}

let unsafeNil: UnsafePointer<Int> = nil
if unsafeNil == nil {
  println("ok unsafeNil == nil")
  // CHECK: ok unsafeNil == nil
}

let removed = NSFileManager.defaultManager().removeItemAtURL(NSURL(string:"/this/file/does/not/exist"), error:nil)
if !removed {
  println("ok !removed")
  // CHECK: ok !removed
}

var selNil: Selector = nil
if selNil == nil { 
  println("ok selNil == nil")
  // CHECK: ok selNil == nil
}
selNil = Selector(nil)
if selNil == nil { 
  println("ok selNil == nil")
  // CHECK: ok selNil == nil
}

