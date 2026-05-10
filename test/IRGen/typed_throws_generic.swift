// RUN: %target-swift-frontend -primary-file %s -emit-irgen
// RUN: %target-swift-frontend -primary-file %s -emit-irgen -enable-library-evolution

// https://github.com/swiftlang/swift/issues/80020
//
// We used to assert if you had a loadable return type that contained
// a generic parameter, and typed throws.

public enum MyError: Error {
  case error
}

public struct G<T> {}  // Note: G<T> is loadable

public func loadableReturnType<T>(_ b: Bool, _ t: T) throws(MyError) -> G<T> {
   if b {
    throw MyError.error
  } else {
    return G<T>()
  }
}

public func loadableReturnType2<U>(_ b: Bool, _ u: U?) throws(MyError) -> G<U?> {
  if b {
    throw MyError.error
  } else {
    return try loadableReturnType(b, u)
  }
}

// https://github.com/swiftlang/swift/issues/74289
public struct LoadableGenericError<E>: Error {
  struct Nested: Error {}
}

func throwsLoadableGeneric1<E>(_ b: Bool, _: E) throws(LoadableGenericError<E>) {
  if b {
    throw LoadableGenericError<E>()
  }
}

func throwsLoadableGeneric2<E>(_ b: Bool, _: E) throws(LoadableGenericError<E>.Nested) {
  if b {
    throw LoadableGenericError<E>.Nested()
  }
}

// https://github.com/swiftlang/swift/issues/86347
func asyncThrowsLoadableGeneric1<E>(_ b: Bool, _: E) async throws(LoadableGenericError<E>) {
  if b {
    throw LoadableGenericError<E>()
  }
}

func asyncThrowsLoadableGeneric2<E>(_ b: Bool, _: E) async throws(LoadableGenericError<E>.Nested) {
  if b {
    throw LoadableGenericError<E>.Nested()
  }
}

// Make sure we do the right thing in dispatch thunks, too

public class ClassTest {
  public func throwsLoadableGeneric1<E>(_ b: Bool, _: E) throws(LoadableGenericError<E>) {}
  public func asyncThrowsLoadableGeneric<E>(_ b: Bool, _: E) async throws(LoadableGenericError<E>) {}
}

public protocol ProtocolTest {
  func throwsLoadableGeneric<E>(_ b: Bool, _: E) throws(LoadableGenericError<E>)
  func asyncThrowsLoadableGeneric<E>(_ b: Bool, _: E) async throws(LoadableGenericError<E>)
}