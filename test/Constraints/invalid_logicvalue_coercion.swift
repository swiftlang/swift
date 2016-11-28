// RUN: %target-typecheck-verify-swift

class C {}
var c = C()
if c as C { // expected-error{{'C' is not convertible to 'Bool'}}
}

if ({1} as () -> Int) { // expected-error{{'() -> Int' is not convertible to 'Bool'}}
}
