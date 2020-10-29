// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/FortyTwo.swiftmodule -I %S/Inputs %s -enable-cxx-interop

// REQUIRES: SR-13785

@_implementationOnly import UserA
import UserB

@_inlineable
public func createAWrapper() {
  let _wrapper = MagicWrapper()
}
