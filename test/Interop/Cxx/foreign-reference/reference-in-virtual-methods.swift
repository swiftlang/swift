// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs %s -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple

import VirtMethodWithRvalRef

func f(_ x: CxxForeignRef, _ y: NonTrivial) {
    x.takesRValRef(consuming: y)
}
