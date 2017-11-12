// RUN: %target-swift-frontend -typecheck -verify %s

class C {}
class D {}

func f1(f : (inout Int) -> ()) {}

func f2(f : (inout Any) -> ()) {
    // TODO: Error here is still pretty bad...
    f1 { f(&$0) } // expected-error{{conversion from 'Int' to 'Any'}}
}
