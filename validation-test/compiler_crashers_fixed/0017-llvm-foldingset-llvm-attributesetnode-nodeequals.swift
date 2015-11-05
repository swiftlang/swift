// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/jvasileff (John Vasileff)
// This bug is NOT triggered when compiling with -O.

func f<T : Boolean>(b: T) {
}
f(true as Boolean) // expected-error {{cannot invoke 'f' with an argument list of type '(Boolean)'}} // expected-note {{expected an argument list of type '(T)'}}
