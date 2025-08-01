// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging %s -cxx-interoperability-mode=default -disable-availability-checking

import VirtMethodWithRvalRef

func f(_ x: CxxForeignRef, _ y: NonTrivial) {
    x.takesRValRef(consuming: y)
}
