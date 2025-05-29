// RUN: %target-swift-frontend -emit-ir %s

public struct G<T> {
  var value: T { fatalError() }
}

public func f<T>(transform: () -> T) {}

public func g<T, each V>(_ v: repeat G<each V>?, transform: (repeat (each V)?) -> T) {
  f(transform: { transform(repeat (each v)?.value ?? nil) })
}
