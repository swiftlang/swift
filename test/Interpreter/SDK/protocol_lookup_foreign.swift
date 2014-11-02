// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: sdk

// TODO: Write these using "x as P" casts when we support that.

@asmname("swift_stdlib_dynamicCastToExistential1_2")
func castToProtocol<SourceType, DestType>(
    value: SourceType,
    _: DestType.Type
) -> DestType?

import Foundation

protocol Fooable {
  func foo()
}

func fooify<T>(x: T) {
  if let foo = castToProtocol(x, Fooable.self) {
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

fooify(NSRect()) // CHECK: NSRect
fooify(NSPoint()) // CHECK-NEXT: not fooable
fooify(CFSetCreate(kCFAllocatorDefault, nil, 0, nil)!) // CHECK-NEXT: CFSet
fooify(CFArrayCreate(kCFAllocatorDefault, nil, 0, nil)!) // CHECK-NEXT: not fooable

// TODO: objc classes
