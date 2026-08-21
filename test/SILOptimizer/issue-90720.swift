// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil %s -o /dev/null

// Regression test for https://github.com/swiftlang/swift/issues/90720
//
// When FunctionSignatureOpts specializes a no-return function (here: the
// implicit closure for the unapplied reference to `crash`), the rewritten
// thunk is terminated with an `unreachable`, which cuts off lifetimes and
// requires them to be completed before the pass finishes. This used to hit
// the "didn't complete lifetimes" assertion in the pass manager.

struct Hooks {
    static var handler: (() -> Never)?
    static func crash() -> Never { fatalError() }
    static func reset() { handler = crash }
}
