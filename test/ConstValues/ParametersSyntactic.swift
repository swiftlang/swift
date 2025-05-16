// Constant globals on simple integer literals
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %target-swift-frontend -emit-sil -primary-file %s -verify -enable-experimental-feature CompileTimeValues

func bar(@const _ thing: UInt) -> UInt {
    return thing
}

func foo() {
    let _ = bar(42)
    let _ = bar(0xf000f000)
    #if _pointerBitWidth(_64)
    let _ = bar(0xf000f000_f000f000)
    #endif
    let _ = bar(UInt.random(in: 0..<10))
    // expected-error@-1 {{expected a compile-time value argument for a '@const' parameter}}
    // expected-error@-2 {{not supported in a '@const' expression}}
}
