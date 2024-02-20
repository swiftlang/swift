// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import _Differentiation
import StdlibUnittest

// When the original function contains loops, we allocate a context object 
// on the heap. This context object may store non-trivial objects, such as closures,
// that need to be freed explicitly, at program exit. This test verifies that the 
// autodiff runtime destroys and deallocates any such objects.

extension LifetimeTracked: AdditiveArithmetic {
    public static var zero: LifetimeTracked { fatalError() }
    public static func + (lhs: LifetimeTracked, rhs: LifetimeTracked) -> LifetimeTracked {fatalError()}
    public static func - (lhs: LifetimeTracked, rhs: LifetimeTracked) -> LifetimeTracked {fatalError()}
}

extension LifetimeTracked: Differentiable {
        public typealias TangentVector = LifetimeTracked
        public func move(by: LifetimeTracked) {fatalError()}
}

extension LifetimeTracked {
    // The original differentiable callee. 
    func callee(_: Float) -> Float { 42 }

    // The callee differential (pullback in this case), that is
    // captured in the context object allocated on the heap in the
    // presence of loops. 
    // 
    // If the autodiff runtime does not free this callee differential 
    // properly, the `LifetimeTracked` instance that it captures will 
    // also not be freed and we will have a detectable memory leak.
    @derivative(of: callee, wrt: (self, f))
    func calleeDifferential(f: Float) -> (value: Float, pullback: (Float) -> (LifetimeTracked, Float)) {
        return (
            value: f, 
            pullback: { x in (self, x) }
        )
    }
}

@differentiable(reverse)
func f(ltti: LifetimeTracked) -> Float {
    for _ in 0..<1 {
    }
    return ltti.callee(0xDEADBEE)
}

var Tests = TestSuite("CalleeDifferentialLeakTest")

Tests.test("dontLeakCalleeDifferential") {
  do {
    let ltti = LifetimeTracked(0xDEADBEE)
    let _ = valueWithPullback(at: ltti, of: f)
  }
  expectEqual(0, LifetimeTracked.instances)
}

runAllTests()