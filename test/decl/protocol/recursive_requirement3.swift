// RUN: %swift -parse %s -verify

protocol PP {
  typealias A : P = Self
}

protocol P : PP {
  typealias A = Self
}

struct X<T: P> {
}

struct Y : P {
  typealias A = Y
}

func f<T: P>() {
 let x = X<T.A>()
}