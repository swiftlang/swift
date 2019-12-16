// RUN: %target-typecheck-verify-swift

class C {}
var c = C()
if c as C { // expected-error{{cannot convert value of type 'C' to expected condition type 'Bool'}} expected-warning {{redundant cast to 'C' has no effect}} {{6-11=}}
}

if ({1} as () -> Int) { // expected-error{{cannot convert value of type '() -> Int' to expected condition type 'Bool'}}
}
