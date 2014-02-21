// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

val opaqueNil: COpaquePointer = nil
if opaqueNil == nil {
  println("ok opaqueNil == nil")
  // CHECK: ok opaqueNil == nil
}

val unsafeNil: UnsafePointer<Int> = nil
if unsafeNil == nil {
  println("ok unsafeNil == nil")
  // CHECK: ok unsafeNil == nil
}

val removed = NSFileManager.defaultManager().removeItemAtURL(NSURL(withString:"/this/file/does/not/exist"), error:nil)
if !removed {
  println("ok !removed")
  // CHECK: ok !removed
}
