// RUN: %target-swift-frontend %s -typecheck -verify -enable-experimental-cxx-interop
// RUN: %target-swift-frontend %s -typecheck -verify -cxx-interoperability-mode=swift-6
// RUN: %target-swift-frontend %s -typecheck -verify -cxx-interoperability-mode=upcoming-swift

// REQUIRES: OS=macosx || OS=linux-gnu || OS=freebsd

import CxxStdlib

func test() {
    let x: Double = 1.0
    let y: Double = 2.0

    let _ =  pow(x, y)

    let _ = abs(x)
    // https://github.com/apple/swift/issues/67006
    // let _ = div(42, 2)
    let _ = sin(x)
    let _ = cos(x)

    // strstr comes from stdlib.h or cstdlib on *some* flavors of linux, so it
    // won't get imported. We may need a more fine grained approach for those
    // platforms.
#if !os(Linux)
    let _ = strstr("a", "aaa")
#endif

    exit(0)
}
