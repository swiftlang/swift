// RUN: %target-typecheck-verify-swift

// This test validates that move only enums cannot be marked indirect or have
// indirect cases.

struct S: ~Copyable {
    var i = 5
}

enum E: ~Copyable { }

enum E1: ~Copyable {
    case first
    case second(S)
}

indirect enum E2: ~Copyable { // expected-error {{noncopyable enum 'E2' cannot be marked indirect or have indirect cases yet}}
    case first
    case second(S)
}

enum E3: ~Copyable {
    case first
    indirect case second(S) // expected-error {{noncopyable enum 'E3' cannot be marked indirect or have indirect cases yet}}
}
