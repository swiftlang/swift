// RUN: %target-swift-frontend -typecheck -verify %s

class C {}
class D {}

func f1(f : (inout Int) -> ()) {}

func f2(f : (inout Any) -> ()) {
    f1 { f(&$0) } // expected-error{{inout argument could be set to a value with a type other than 'Int'; use a value declared as type 'Any' instead}}
}
