// RUN: %target-typecheck-verify-swift -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_private.modulemap -target %target-stable-abi-triple -swift-version 5 -enable-library-evolution
// REQUIRES: objc_interop

@_implementationOnly import objc_implementation_internal

@objc @implementation extension InternalObjCClass {
  @objc public func method(fromHeader1: CInt) {
    // OK
  }
}
