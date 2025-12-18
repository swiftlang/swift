// RUN: %target-swift-frontend -emit-ir %s -disable-objc-attr-requires-foundation-module
// REQUIRES: objc_interop

// https://github.com/apple/swift/issues/43937

@objc protocol TestProtocol {
    func foo(i: Int)
}

class Test : TestProtocol {
    @objc func foo(i: Int) {
    }
}

func bar(t: TestProtocol?) {
    let foofunc = t?.foo
    foofunc?(5)
}
