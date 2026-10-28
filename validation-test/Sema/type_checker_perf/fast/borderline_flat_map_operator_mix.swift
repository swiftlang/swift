// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200 -language-mode 4 -solver-enable-promote-supertypes
// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200 -language-mode 5 -solver-enable-promote-supertypes
// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000 -language-mode 5 -solver-disable-promote-supertypes

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

