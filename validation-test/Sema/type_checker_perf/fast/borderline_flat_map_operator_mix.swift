// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=10

// REQUIRES: no_asan

struct S {
    var t: Double

    init(_ t: Double) {
       self.t = t
    }
}

func f(x: Array<S>, y: Range<Int>) -> [S] {
    return x.flatMap { z in
        return ((y.lowerBound / 1)...(y.upperBound + 1) / 1).flatMap { w in
            return [S(1 * Double(w) + 1.0 + z.t),
                    S(1 * Double(w) + 1.0 - z.t)]
        }
    }
}

