// RUN: %target-swift-emit-silgen -verify %s

protocol P { var x: Int { get } }

extension Int: P { var x: Int { return self } }

// rdar://problem/63187509: Evaluating the variable initializer for `px`
// requires allocating a temporary stack slot for the address only value of
// `Butt.p`. Ensure that this gets cleaned up appropriately (which is asserted
// by the SIL verifier).
struct Butt {
    static var p: P = 0

    let px = Butt.p.x

    let y: Int
}

