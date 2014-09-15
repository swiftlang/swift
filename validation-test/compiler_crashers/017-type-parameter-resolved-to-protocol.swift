// RUN: %swift -emit-ir %s -verify
// Test case submitted to project by https://github.com/jvasileff (John Vasileff)
// This bug is NOT triggered when compiling with -O.

func f<T : BooleanType>(b: T) { // expected-note{{in call to function 'f'}}
}

f(true as BooleanType) // expected-error{{generic parameter 'T' cannot be bound to non-@objc protocol type 'BooleanType'}}

