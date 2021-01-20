// Check that IRGen doesn't crash. rdar://72999296
// RUN: %target-swift-emit-ir %s -I %S/Inputs

import AtomicBoolModule

public func f() -> MyAtomicBool {
  return MyAtomicBool()
}

public func g(_ b: MyAtomicBool) {
  let _ = b
}
