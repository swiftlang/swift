// RUN: not --crash %target-swift-emit-ir %s -I %S/Inputs

import AtomicBoolModule

public func f() -> MyAtomicBool {
  return MyAtomicBool()
}

public func g(_ b: MyAtomicBool) {
  let _ = b
}
