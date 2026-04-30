// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs %s -cxx-interoperability-mode=default -disable-availability-checking

import VirtMethodWitMoveOnly

func f(_ x: CxxForeignRef, _ y: consuming MoveOnly) {
    x.takesMoveOnly(y)
}
