// RUN: %target-swift-emit-silgen %s

// https://github.com/swiftlang/swift/issues/62061

protocol P {}

class C<T> {}

protocol Q {
  associatedtype A
}

func foo1<T>(t: T) {
  let _: (any P & C<T>) -> T = { x in t }
}

func foo2<T: Q>(t: T) {
  let _: (any P & C<(T.A) -> ()>) -> T = { x in t }
}

