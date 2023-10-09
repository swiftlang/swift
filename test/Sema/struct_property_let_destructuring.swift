// RUN: %target-swift-frontend -typecheck -verify %s

// https://github.com/apple/swift/issues/68915
// Destructuring initializations for `let` properties in structs isn't
// implemented correctly in SILGen, so diagnose it as unsupported for now.

struct Foo {
    var value: Int = 42

    let (aaa, bbb) = ("aaa", "bbb") // expected-error{{unsupported}}

    let (z1, z2, z3) = ("one", 1, Double.pi) // expected-error{{unsupported}}


    func tellMe() {
        print(foo.aaa)
        print(foo.bbb)          // output:  aaa


        assert(aaa == "aaa")
        assert(bbb == "bbb", "bbb should be bbb but it's \(bbb)")
    }

}


let foo = Foo(/*value: 1*/)


foo.tellMe()




print("Hello")
