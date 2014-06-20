// RUN: %swift -parse %s -verify

protocol P0 {
 typealias A: P = Self
}

protocol P : P0 {}

protocol DeclaredP : P0, P {}

struct Y : DeclaredP {
}

struct X<T:P> {}

func f<T:P>(a: T) {
 let works = X<T.A>()
}

f(Y())
