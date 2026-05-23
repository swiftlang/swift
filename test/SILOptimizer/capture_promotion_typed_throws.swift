// RUN: %target-swift-frontend -emit-sil %s

// https://github.com/swiftlang/swift/issues/86391

struct S {}

func bug<Success: Sendable>(
    _ rez: Success
) {
    var s = S() // must be a var
    doit {
        _ = s
        return rez
    }
    doit {
        use(s)
        return rez
    }
}

func doit<Success, Failure: Error>(
    op: @escaping () throws(Failure) -> Success
) {}

@_optimize(none)
func use<T>(_: T) {}