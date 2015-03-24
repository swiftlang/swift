// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation

protocol Fooable {
  func foo()
}

func fooify<T>(x: T) {
  if let foo? = x as? Fooable {
    foo.foo()
  } else {
    println("not fooable")
  }
}

extension NSRect: Fooable {
  func foo() { println("NSRect") }
}

extension CFSet: Fooable {
  func foo() { println("CFSet") }
}

extension NSString: Fooable {
  func foo() { println("NSString") }
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

