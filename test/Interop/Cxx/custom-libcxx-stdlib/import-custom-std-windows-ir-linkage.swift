// RUN: %target-swiftxx-frontend -emit-ir -Xcc -cxx-isystem -Xcc %S/Inputs/c++ -Xcc -stdlib=libc++ -Xcc -nostdinc++ -I %S/Inputs -Xcc -fignore-exceptions %s | %FileCheck -check-prefix=IR %s

import custom_std

// REQUIRES: OS=windows-msvc

public func testCustomString() -> Int {
    let s = std.string("hello")
    return Int(s.size())
}

// IR: "/DEFAULTLIB:libswiftCxx.lib"
// IR-NOT: "/DEFAULTLIB:libswiftCxxStdlib.lib"
