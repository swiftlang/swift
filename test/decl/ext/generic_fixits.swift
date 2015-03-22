// RUN: %target-parse-verify-swift -diagnose-generic-extensions

protocol P { }

struct X<T> { }
struct Y<T : P, U, V where V : P> { }

extension X { } // expected-warning{{extension of generic type 'X' requires generic parameters}}{{12-12=<T>}}

extension Y { } // expected-warning{{extension of generic type 'Y' requires generic parameters}}{{12-12=<T, U, V>}}

