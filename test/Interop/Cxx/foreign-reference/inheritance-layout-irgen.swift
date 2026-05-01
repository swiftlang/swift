// RUN: %target-swift-emit-ir -Onone %s -I %S/Inputs -cxx-interoperability-mode=default -validate-tbd-against-ir=none -disable-llvm-verify -Xcc -fignore-exceptions -disable-availability-checking

// This ensured we do not crash during IRGen for inherited C++ foreign reference types.

import InheritanceLayout

@inline(never)
public func blackHole<T>(_ _: T) {}

let x = DerivedOutOfOrder.getInstance()

blackHole(x.baseField)
blackHole(x.derivedField)
blackHole(x.leafField)

let y = DerivedUsesBaseTailPadding.getInstance()

blackHole(y.field2)
blackHole(y.field4)
