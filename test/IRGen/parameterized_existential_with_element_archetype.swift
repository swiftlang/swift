// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

public protocol P1<A> {
    associatedtype A
}

public protocol P2<A>: class {
    associatedtype A
}

public func f1<each T>(p: repeat any P1<each T>) {
  let _ = (repeat each p)
}

public func f2<each T>(p: repeat any P2<each T>) {
  let _ = (repeat each p)
}
