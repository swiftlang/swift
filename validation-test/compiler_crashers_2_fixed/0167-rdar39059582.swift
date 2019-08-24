// RUN: %target-swift-frontend %s -emit-ir

protocol X {
  associatedtype R : Y
}

protocol Y {
  associatedtype Q : X where Q.R == Self
}

struct B : Y {
  typealias Q = L<B> 
}

struct L<V : Y> : X {
  typealias R = V
}
