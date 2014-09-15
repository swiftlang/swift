// RUN: %swift -emit-ir %s -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

func ^(a: BooleanType, Bool) -> Bool {
    return !(a) // expected-error{{cannot invoke '!' with an argument of type 'BooleanType'}}
}
