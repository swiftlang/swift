// RUN: %target-swift-frontend %s -typecheck -verify -enable-experimental-cxx-interop -Xcc -std=c++17

// REQUIRES: OS=macosx || OS=linux-gnu

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
    let _ = strstr("a", "aaa")
  
    exit(0)
}
