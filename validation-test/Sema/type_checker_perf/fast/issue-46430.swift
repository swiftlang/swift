// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200

func slow(_ m: [[UInt32]]) {
    var n = m
    for i in 1 ..< 10 {
        for j in 1 ..< 10 {
            n[i][j] = min(n[i - 1][j - 1], n[i - 1][j], n[i][j - 1]) + 1
        }
    }
}

