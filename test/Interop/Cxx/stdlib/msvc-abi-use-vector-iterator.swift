// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xcc -std=c++17)

// REQUIRES: OS=windows-msvc
// REQUIRES: executable_test

import MsvcUseVecIt

func test() -> Bool {
    let result = f()
    let begin = result.pointee.providers.__beginUnsafe()
    let end = result.pointee.providers.__endUnsafe()
    return begin != end
}

let _ = test()
