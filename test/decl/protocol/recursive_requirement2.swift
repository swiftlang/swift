// RUN: %swift -parse %s -verify

protocol P {
 typealias A : P
}

struct X<T: P> {
}

func f<T: P>() {
 let x = X<T.A>()
}
