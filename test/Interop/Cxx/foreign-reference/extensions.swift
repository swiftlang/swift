// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -disable-availability-checking -Onone)
// REQUIRES: executable_test

import ReferenceCounted

protocol MyProto {
    func foo() -> Self
}

extension NS.LocalCount {
    static func g() {}

    public func f() {
        Self.g()
    }
}

extension NS.LocalCount: MyProto {
    public func foo() -> Self {
        return self
    }
}

let x = NS.LocalCount.create()
x.f()
let _ = x.foo()
