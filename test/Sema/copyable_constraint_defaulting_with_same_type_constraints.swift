// RUN: %target-swift-frontend -typecheck -verify %s

struct Bar: ~Copyable {}

struct Foo<T: ~Copyable> {
    func foo() -> T { fatalError() }
}

extension Foo where T == Bar {
    func bar() -> Bar {
        return foo()
    }
}

struct Foo2<T: ~Copyable, U: ~Copyable> {
    func foo1() -> T { fatalError() }
    func foo2() -> U { fatalError() }
}

extension Foo2 where T == Bar, U == Bar {
    func bar1_1() -> Bar {
        return foo1()
    }
    func bar1_2() -> Bar {
        return foo2()
    }
}

extension Foo2 where T == U, U == Bar {
    func bar2_1() -> Bar {
        return foo1()
    }
    func bar2_2() -> Bar {
        return foo2()
    }
}

extension Foo2 where T == Bar, U == T {
    func bar3_1() -> Bar {
        return foo1()
    }
    func bar3_2() -> Bar {
        return foo2()
    }
}

func needsCopyable<T>(_: T) {}

// T and U are still Copyable by default here, since the equivalence class of
// the same-type-constrained parameters is entirely within the scope of the
// extension, and there is no explicit suppression of the default.
extension Foo2 where T == U {
    func test(_ x: T) {
        needsCopyable(x)
    }
}

// Explicitly suppressing Copyable on one type parameter also prevents the other
// parameters in the equivalence class from defaulting to Copyable.
extension Foo2 where T == U, T: ~Copyable {
    func bar4_1() -> T {
        return foo1()
    }
    func bar4_2() -> T {
        return foo2()
    }
}

extension Foo2 where T == U, U: ~Copyable {
    func bar5_1() -> T {
        return foo1()
    }
    func bar5_2() -> T {
        return foo2()
    }
}

