// RUN: %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none

import ReferenceToDerived

func foo(_ x: D) {}
