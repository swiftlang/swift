// RUN: %target-swift-emit-silgen %s -I %S/Inputs -enable-experimental-cxx-interop -disable-availability-checking

import ClassTemplateWithFrt

func f() {
    // Used to trigger error while emitting SIL.
    let _ = MagicWrapperFrt()
    let _ = MagicWrapperConstFrt()
    let _ = MagicWrapperVolatileFrtRef()
}
