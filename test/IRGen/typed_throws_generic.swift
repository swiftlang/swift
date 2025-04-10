// RUN: %target-swift-frontend -primary-file %s -emit-irgen

// https://github.com/swiftlang/swift/issues/80020
//
// We used to assert if you had a loadable return type that contained
// a generic parameter.

public enum MyError: Error {
  case error
}

public struct G<T> {}  // Note: G<T> is loadable

public func f<T>(t: T) throws(MyError) -> G<T> {
  return G<T>()
}

public func g<U>(u: U?) throws(MyError) -> G<U?> {
  return try f(t: u)
}