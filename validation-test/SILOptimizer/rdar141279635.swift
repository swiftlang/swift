// RUN: %target-build-swift %s

protocol P: ~Copyable {
    var property: Bool { get }
    consuming func function()
}

struct S: P, ~Copyable {
    var property: Bool { false }
    consuming func function() {}
}

func g(s: consuming any P & ~Copyable) {
    let s = s
    s.function()
}

func f() {
    let s = S()
    g(s: s)
}
