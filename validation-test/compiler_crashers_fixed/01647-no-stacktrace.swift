// RUN: %target-swift-frontend %s -emit-silgen

// Issue found by https://github.com/radex
// rdar://18851497

struct A {
    private let b: [Int] = []
    func a() {
    }
}
typealias B = (C, C)
enum C {
    case D
    case E
}
func c() {
    let d: (B, A)? = nil
    let (e, f) = d!
    f.a()
}
