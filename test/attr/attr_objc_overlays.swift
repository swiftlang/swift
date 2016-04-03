// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

import Foundation

struct NativeStruct {}

class Test {
  @objc func invalid() -> NativeStruct { return NativeStruct() }
  // expected-error@-1 {{method cannot be marked @objc because its result type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  @objc func selector(_ sel: Selector) {} // no-warning
  @objc func selectorRef(_ sel: UnsafeMutablePointer<Selector>) {} // no-warning
  @objc func boolean(_ b: ObjCBool) {} // no-warning
  @objc func booleanRef(_ ref: UnsafeMutablePointer<ObjCBool>) {} // no-warning
}
