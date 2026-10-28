// RUN: %target-swift-frontend -emit-sil -module-name test -verify %s

// Ensure valid diagnostics is produced, not a crash

import _Differentiation

@differentiable(reverse)
func sumAllRows(_ rows: [[Float]]) -> Float {
    var total: Float = 0
    for i in 0 ..< withoutDerivative(at: rows).count {
        // expected-note @+2 {{cannot differentiate functions that have not been marked '@differentiable' and that are defined in other files}}
        // expected-error @+1 {{expression is not differentiable}}
        total += rows[i].reduce(0, +)
    }
    return total
}

print(valueWithPullback(at: [[1, 2, 3], [4, 5]], of: sumAllRows).value)
