// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

struct Environment {
    let a: () -> Bool
    let b: () -> Bool
    let c: () -> Void
    let d: () -> Void
    let e: (Bool) -> Void
    let f: (Bool) -> Void
}

extension Environment {
    static var test: Self = .init(
        a: mock(),
        b: mock(),
        c: mock(),
        d: mock(),
        e: mock(),
        f: mock()
    )
}

func mock<Z>() -> () -> Z { { fatalError() } }

// 0.4s vs 0.4s
func mock<A, Z>() -> (A) -> Z { { _ in fatalError() } }

// 0.4s vs 0.4s
func mock<A, B, Z>() -> (A, B) -> Z { { _, _ in fatalError() } }

// 0.8s vs 0.5s
func mock<A, B, C, Z>() -> (A, B, C) -> Z { { _, _, _ in fatalError() } }

// 26s vs 0.4s
func mock<A, B, C, D, Z>() -> (A, B, C, D) -> Z { { _, _, _, _ in fatalError() } }

// 281s vs 0.4s
func mock<A, B, C, D, E, Z>() -> (A, B, C, D, E) -> Z { { _, _, _, _, _ in fatalError() } }

// 361s vs 0.3s
func mock<A, B, C, D, E, F, Z>() -> (A, B, C, D, E, F) -> Z { { _, _, _, _, _, _ in fatalError() } }
