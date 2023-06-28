// RUN: %target-swift-frontend %s -typecheck -verify -enable-experimental-cxx-interop -Xcc -std=c++17

// REQUIRES: OS=macosx || OS=linux-gnu

import CxxStdlib

func test() {
    let x: Float = 1.0
    let y: Float = 2.0

    let _ =  pow(x, y)

    let _ = abs(x)
    let _ = div(42, 2)
    let _ = strstr("a", "aaa")
  
    exit(0)
}
