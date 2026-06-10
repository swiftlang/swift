// RUN: %target-typecheck-verify-swift -solver-scope-threshold=150

// This took ~2000 scopes in 6.2 and ~240k scopes in 6.3.

struct S1 {
    var x: Int
    var y: Int
    var z: Int
}

struct S2 {
    var x: Int32
    var y: Int32
    var z: Int32
}

func test(u: [Int32], v: [Int32]) {
    let _ = stride(from: 0, to: u.count, by: 3).map {
        S2(x: u[$0 + 1], y: u[$0], z: u[$0 + 2])
    }

    let _ = stride(from: 0, to: v.count, by: 3).map {
        S1(x: Int(v[$0]), y: Int(v[$0 + 1]), z: Int(v[$0 + 2]))
    }
}
