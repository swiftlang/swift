// RUN: %target-typecheck-verify-swift

// rdar://problem/54296278 - infinite loop
class Foo {}
class Bar: Bar {} // expected-error{{'Bar' inherits from itself}}
func foo(_ o: AnyObject) -> Foo? {
    return o as? Bar // expected-error{{cannot convert return expression of type 'Bar?' to return type 'Foo?'}}
}
