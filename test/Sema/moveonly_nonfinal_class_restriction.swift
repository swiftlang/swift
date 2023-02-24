// RUN: %target-typecheck-verify-swift -enable-experimental-move-only

// This test validates that we do not allow for non-final classes to contain
// move only fields. This is just a temporary measure.

@_moveOnly
struct S {
    var i: Int = 5
}

class C {
    var s = S() // expected-error {{non-final classes containing move only fields is not yet supported}}
    let s1 = S() // expected-error {{non-final classes containing move only fields is not yet supported}}
    var s2: S // expected-error {{non-final classes containing move only fields is not yet supported}}
    let s3: S // expected-error {{non-final classes containing move only fields is not yet supported}}

    init() {
        s2 = S()
        s3 = S()
    }
}

final class C2 {
    var s = S()
    let s1 = S()
    var s2: S
    let s3: S

    init() {
        s2 = S()
        s3 = S()
    }
}
