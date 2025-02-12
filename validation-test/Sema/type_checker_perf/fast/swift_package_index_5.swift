// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=1000 -swift-version 5
// REQUIRES: tools-release,no_asan

func g<T>(_: T) throws {
    let _: T? =
        (try? f() as? T) ??
        (try? f() as? T) ??
        (try? f() as? T) ??
        (try? f() as? T) ??
        (try? f() as? T)
}

func f() throws -> Int { return 0 }
