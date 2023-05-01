// RUN: %target-swift-frontend %s -typecheck -verify -enable-experimental-cxx-interop -Xcc -std=c++17

// REQUIRES: OS=macosx || OS=linux-gnu

import CxxStdlib

func test() {
    let x: Float = 1.0
    let y: Double = 2.0

    // Note: we dispatch `pow(Float,Double)`
    // to ensure we don't pick up the
    // C++ stdlib `pow` function template.
    // The `pow` function is still reexported
    // from Darwin via CxxStdlib, so there are
    // matching overloads that can be found still.
    // Note: the error is different on Glibc instead
    // of Darwin, so do not check the exact error.
    let _ =  CxxStdlib.pow(x, y) // expected-error {{}}

    let _ = CxxStdlib.abs(x) // expected-error {{module 'CxxStdlib' has no member named 'abs'}}
    let _ = CxxStdlib.div(x) // expected-error {{module 'CxxStdlib' has no member named 'div'}}
}
