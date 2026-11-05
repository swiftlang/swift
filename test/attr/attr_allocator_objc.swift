// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators -target %target-future-triple

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

@allocator(Pool)
class SwiftClass {}

// Native Swift classes may be exposed to Objective-C.
@allocator(Pool)
@objcMembers
class SwiftClassWithObjCMembers {
  func f() {}
}
