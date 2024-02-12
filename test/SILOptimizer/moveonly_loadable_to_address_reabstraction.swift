// RUN: %target-swift-frontend -emit-sil -verify -enable-library-evolution %s

// Verify that call sequences that require reabstracting a noncopyable value
// from a loadable representation to an in-memory one are properly allowed by
// the move-only checker.

public struct Foo: ~Copyable {
    private var x: String = ""

    public var isEmpty: Bool {
        @_silgen_name("get_isEmpty") get
    }
}

public struct Bar: ~Copyable {
    public var x = Foo()

    public consuming func foo() {
        // `Foo` is internally a loadable type, but it's public with library
        // evolution enabled, so public members like `isEmpty` take their
        // argument indirectly.
        if x.isEmpty {}
        //else { bar(x) }
    }
}

@_silgen_name("bar")
func bar(_: consuming Foo)

func foo(_ x: consuming Foo) {
    // `[String]` is a loadable copyable type, but we're using the
    // `consuming` modifier on `x` to suppress implicit copyability.
    // `isEmpty` is a method from the Collection protocol, so it takes `self`
    // generically and therefore indirectly.
    if x.isEmpty {}
    else { bar(x) }
}

func copyableBar(_: consuming [String]) {}

func copyableFoo(prefix: consuming [String]) {
    if  prefix.isEmpty {  }
    else { copyableBar(prefix) }
}

struct CopyableFoo {
    var prefix: [String]

    consuming func foo() {
        if  prefix.isEmpty {  }
        else { copyableBar(prefix) }
    }
}
