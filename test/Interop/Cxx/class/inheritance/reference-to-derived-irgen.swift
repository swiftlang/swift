// RUN: %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none
// We should not fail with a circular reference error here.

import ReferenceToDerived

func foo(_ x: D) {}
