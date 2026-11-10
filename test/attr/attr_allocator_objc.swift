// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators -target %target-future-triple
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name main -default-class-allocator main.Pool -verify-additional-prefix default-

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: objc_interop

import Foundation

enum Pool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

// Objective-C classes are allocated and freed by the Objective-C runtime.
@allocator(Pool) // expected-error {{'@allocator' cannot be applied to class 'ObjCClass' because it inherits from an Objective-C class}}
class ObjCClass: NSObject {}

// They may opt out of -default-class-allocator.
@allocator(SystemClassAllocator)
class OptedOutObjCClass: NSObject {}

// Under -default-class-allocator, they must.
class DefaultedObjCClass: NSObject {} // expected-default-error {{default class allocator 'Pool' cannot be applied to class 'DefaultedObjCClass' because it inherits from an Objective-C class}}
// expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}

class DerivedFromOptedOut: OptedOutObjCClass {}

@allocator(Pool)
class SwiftClass {}

// Native Swift classes may be exposed to Objective-C.
@allocator(Pool)
@objcMembers
class SwiftClassWithObjCMembers {
  func f() {}
}
