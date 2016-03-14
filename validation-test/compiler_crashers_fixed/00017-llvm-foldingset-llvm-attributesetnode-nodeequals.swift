// RUN: %target-swift-frontend %s -parse -verify

// Issue found by https://github.com/jvasileff (John Vasileff)
// This bug is NOT triggered when compiling with -O.

func f<T : Boolean>(b: T) {
}
f(true as Boolean) // expected-error {{cannot invoke 'f' with an argument list of type '(Boolean)'}} // expected-note {{expected an argument list of type '(T)'}}
