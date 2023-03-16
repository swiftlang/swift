// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all)

// REQUIRES: executable_test

import StdlibUnittest

defer { runAllTests() }

var Tests = TestSuite("MoveOnlyTests")

@_moveOnly
struct FD {
    var a  = LifetimeTracked(0)

    deinit {
    }
}

Tests.test("simple deinit called once") {
    do {
        let s = FD()
    }
    expectEqual(0, LifetimeTracked.instances)
}

Tests.test("ref element addr destroyed once") {
    class CopyableKlass {
        var fd = FD()
    }

    func assignCopyableKlass(_ x: CopyableKlass) {
        x.fd = FD()
    }

    do {
        let x = CopyableKlass()
        assignCopyableKlass(x)
    }
    expectEqual(0, LifetimeTracked.instances)
}

var global = FD()

Tests.test("global destroyed once") {
    do {
        global = FD()
    }
    expectEqual(0, LifetimeTracked.instances)    
}
