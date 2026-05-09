// RUN: %target-typecheck-verify-swift

// Regression test: the C++-side trailing-closure-in-stmt-condition fix-it
// produced range endpoints that, after mapping through the Swift syntax
// bridge in `swift_ASTGen_addQueuedDiagnostic`, were ordered start > end.
// Root cause: the implicit `callAsFunction` member ref synthesized in
// CSApply was anchored at `args->getStartLoc()`, which for a bare trailing
// closure is the `{` of the closure. That bled into
// `fixItEncloseTrailingClosure`'s `lastLoc` calculation and produced a
// replacement range landing inside the closure, which trapped during
// diagnostic emission.

struct Lock {
    @discardableResult
    func callAsFunction<T>(_ block: () throws -> T) rethrows -> T {
        try block()
    }
}

class Foo {
    private let lock = Lock()
    private var cache: [String: Int] = [:]

    func lookup(key: String) -> Int? {
        if let value = lock { self.cache[key] } { // expected-warning {{trailing closure in this context is confusable with the body of the statement}} {{28-29=(}} {{48-48=)}}
            return value
        }
        return nil
    }
}
