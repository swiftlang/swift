// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1

// REQUIRES: no_asan

struct S {
    var t: Double

    init(_ t: Double) {
       self.t = t
    }
}

// Note: One possible approach to this issue would be to determine when the array literal inside of the inner closure
// doesn't have any other possible bindings but Array and attempt it at that point. That would fail overload of flatMap
// that returns an optional value.
func f(x: Array<S>, y: Range<Int>) -> [S] {
    return x.flatMap { z in // expected-error {{the compiler is unable to type-check this expression in reasonable time}}
        return ((y.lowerBound / 1)...(y.upperBound + 1) / 1).flatMap { w in
            return [S(1 * Double(w) + 1.0 + z.t),
                    S(1 * Double(w) + 1.0 - z.t)]
        }
    }
}

