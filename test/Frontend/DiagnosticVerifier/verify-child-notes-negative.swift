// Tests for error cases in -verify-child-notes mode.

// RUN: not %target-typecheck-verify-swift -verify-child-notes 2>&1 | %FileCheck %s --implicit-check-not error: --implicit-check-not warning: --implicit-check-not note:

// Non-note classification in children block is a parse error.
struct NonNote {}
struct NonNote {} // expected-error {{invalid redeclaration of 'NonNote'}} {{children: expected-error {{wrong}} expected-note@-1{{'NonNote' previously declared here}} }}
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: only notes allowed in child diagnostics block

// Mismatched child note message leaves the child note unmatched.
struct WrongMsg {}
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: unexpected child note produced: 'WrongMsg' previously declared here
// CHECK: [[@LINE+1]]:{{[0-9]+}}: note: for parent matched here
struct WrongMsg {} // expected-error {{invalid redeclaration of 'WrongMsg'}} {{children: expected-note@-3 {{wrong message}} }}
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: expected note not produced

struct NoParent {}
struct NoParent {}
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: unexpected error produced: invalid redeclaration of 'NoParent'
// CHECK: [[@LINE-3]]:{{[0-9]+}}: note: with child note: 'NoParent' previously declared here

enum WithNone { case none }

func testAmbiguous1(_ x: WithNone?) {
    switch x {
    case .none: // expected-warning {{assuming you mean 'Optional<WithNone>.none'; did you mean 'WithNone.none' instead?}}
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: unexpected child note produced: use 'nil' to silence this warning
// CHECK: [[@LINE-2]]:{{[0-9]+}}: note: for parent matched here
// CHECK: [[@LINE-3]]:{{[0-9]+}}: error: unexpected child note produced: use 'none?' instead
// CHECK: [[@LINE-4]]:{{[0-9]+}}: note: for parent matched here
        break
    default:
        break
    }
}
func testAmbiguous2(_ x: WithNone?) {
    switch x {
    case .none:
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: unexpected warning produced: assuming you mean 'Optional<WithNone>.none'; did you mean 'WithNone.none' instead?
// CHECK: [[@LINE-2]]:{{[0-9]+}}: note: with child note: use 'nil' to silence this warning
// CHECK: [[@LINE-3]]:{{[0-9]+}}: note: with child note: use 'none?' instead
        break
    default:
        break
    }
}

