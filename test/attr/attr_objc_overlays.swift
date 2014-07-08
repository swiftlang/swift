// RUN: %swift %s %clang-importer-sdk -verify

import ObjectiveC

struct NativeStruct {}

class Test {
  @objc func invalid() -> NativeStruct { return NativeStruct() }
  // expected-error@-1 {{method cannot be marked @objc because its result type cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  @objc func selector(sel: Selector) {} // no-warning
  @objc func selectorRef(sel: UnsafePointer<Selector>) {} // no-warning
  @objc func boolean(b: ObjCBool) {} // no-warning
  @objc func booleanRef(ref: UnsafePointer<ObjCBool>) {} // no-warning
}
