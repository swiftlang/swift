// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Onone) | %FileCheck %s
// REQUIRES: executable_test

// Metadata for foreign reference types is not supported on Windows.
// UNSUPPORTED: OS=windows-msvc

import ReferenceCounted

protocol MyProto {
    static func g()
    func foo() -> Self
    func bar(_ x: Self)
    func baz()
}

extension NS.LocalCount: MyProto {
    static func g() {
        print("Static method g called")
    }

    public func foo() -> Self {
        Self.g()
        return self
    }

    public func bar(_ x: Self) {
    }
}

extension MyProto {
    func baz() {
        Self.g()
    }
}

extension NS.LocalCount {
    public func f() {
        Self.g()
    }
}


let x = NS.LocalCount.create()
x.f()
// CHECK: Static method g called
let _ = x.foo()
// CHECK-NEXT: Static method g called
let _ = x.baz()
// CHECK-NEXT: Static method g called
x.bar(x)
