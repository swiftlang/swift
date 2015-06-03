// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// rdar://20990451 is tracking the fix for compiling this test optimized.
// XFAIL: swift_test_mode_optimize
// XFAIL: swift_test_mode_optimize_unchecked

import Foundation

protocol Fooable {
  func foo()
}

func fooify<T>(x: T) {
  if let foo = x as? Fooable {
    foo.foo()
  } else {
    print("not fooable")
  }
}

extension NSRect: Fooable {
  func foo() { print("NSRect") }
}

extension CFSet: Fooable {
  func foo() { print("CFSet") }
}

extension NSString: Fooable {
  func foo() { print("NSString") }
}

fooify(NSRect()) // CHECK: NSRect
fooify(NSPoint()) // CHECK-NEXT: not fooable
// FIXME: CF types get their ObjC class dynamically looked up during dynamic
// casting.
fooify(CFSetCreate(kCFAllocatorDefault, nil, 0, nil)!) // TODO-NEXT: CFSet CHECK-NEXT: not fooable
fooify(CFArrayCreate(kCFAllocatorDefault, nil, 0, nil)!) // CHECK-NEXT: not fooable
fooify(NSString()) // CHECK-NEXT: NSString
fooify(NSMutableString()) // CHECK-NEXT: NSString
fooify(NSSet()) // CHECK-NEXT: not fooable

