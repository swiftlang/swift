// RUN: %target-typecheck-verify-swift

class C {}
var c = C()
if c as C { // expected-error{{cannot convert value of type 'C' to expected condition type 'Bool'}}
}

if ({1} as () -> Int) { // expected-error{{cannot convert value of type '() -> Int' to expected condition type 'Bool'}}
}
