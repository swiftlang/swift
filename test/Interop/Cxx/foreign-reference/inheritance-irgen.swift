// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop -Xcc -fignore-exceptions -disable-availability-checking

// This ensured we do not crash during IRGen for inherited C++ foreign reference types.

import Inheritance

@inline(never)
public func blackHole<T>(_ _: T) {}

let x = DerivedOutOfOrder.getInstance()

blackHole(x.baseField)
blackHole(x.derivedField)
blackHole(x.leafField)
