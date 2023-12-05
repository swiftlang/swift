// --------------------------------
// A Swift × Haskell stdlib flavour
// --------------------------------

/// MARK: GHC.Show

public protocol Show: ~Copyable {
  borrowing func show() -> String
}

public func print(_ s: borrowing some Show & ~Copyable) {
  print(s.show())
}

/// MARK: Data.Eq

public protocol Eq: ~Copyable {
  static func ==(_ a: borrowing Self, _ b: borrowing Self) -> Bool
  static func /=(_ a: borrowing Self, _ b: borrowing Self) -> Bool
}

public extension Eq {
  static func /=(_ a: borrowing Self, _ b: borrowing Self) -> Bool {
    return !(a == b)
  }
}

public extension Eq where Self: Equatable {
  static func ==(_ a: borrowing Self, _ b: borrowing Self) -> Bool {
    return a == b
  }
}

/// MARK: Iteration

public protocol Generator: ~Copyable {
  associatedtype Element: ~Copyable
  func next() -> Element
}


public struct Vector<T: ~Copyable> {

  subscript(_ i: UInt) -> T {
    fatalError("todo")
  }
}


/// MARK: Data.Maybe

public enum Maybe<Value: ~Copyable> {
  case just(Value)
  case nothing
}

extension Maybe: Show {
  public borrowing func show() -> String {
    fatalError("need borrowing switches")
  }
}

extension Maybe: Eq where Value: Eq {
  public static func ==(_ a: borrowing Self, _ b: borrowing Self) -> Bool {
    fatalError("need borrowing switches")
  }
}

public func maybe<A: ~Copyable, B>(_ defaultVal: B,
                                   _ fn: (consuming A) -> B)
                                   -> (consuming Maybe<A>) -> B {
  return { (_ mayb: consuming Maybe<A>) -> B in
    switch consume mayb {
      case .just(_):
        fatalError("waiting for bugfix here")
        // return fn(val)
      case .nothing:
        return defaultVal
    }
  }
}

@inlinable
public func fromMaybe<A>(_ defaultVal: A) -> (Maybe<A>) -> A {
  return maybe(defaultVal, {$0})
}

public func isJust<A: ~Copyable>(_ m: borrowing Maybe<A>) -> Bool {
  fatalError("need borrowing switches")
}

@inlinable
public func isNothing<A: ~Copyable>(_ m: borrowing Maybe<A>) -> Bool {
  return !isJust(m)
}
