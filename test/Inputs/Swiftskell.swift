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

public extension Eq where Self: ~Copyable {
  static func /=(_ a: borrowing Self, _ b: borrowing Self) -> Bool {
    return !(a == b)
  }
}

/// MARK: Result
public enum Either<Success: ~Copyable, Failure: Error>: ~Copyable {
  case success(Success)
  case failure(Failure)
}

extension Either: Copyable {}

extension Either where Failure == Swift.Error {
  public init(catching body: () throws -> Success) {
    do {
      self = .success(try body())
    } catch {
      self = .failure(error)
    }
  }
}

/// MARK: Iteration

public protocol Generator: ~Copyable {
  associatedtype Element: ~Copyable
  func next() -> Maybe<Element>
}

// MARK: Tuples
public enum Pair<L: ~Copyable, R: ~Copyable>: ~Copyable {
  case elms(L, R)
}

/// MARK: Data.Maybe

public enum Maybe<Value: ~Copyable>: ~Copyable {
  case just(Value)
  case nothing
}

extension Maybe: Copyable {}

extension Maybe: Show where Value: Show & ~Copyable {
  public borrowing func show() -> String {
    switch self {
      case let .just(borrowing elm):
        return elm.show()
      case .nothing:
        return "<nothing>"
    }
  }
}

extension Maybe: Eq where Value: Eq, Value: ~Copyable {
  public static func ==(_ a: borrowing Self, _ b: borrowing Self) -> Bool {
    switch a {
      case let .just(borrowing a1):
        switch b {
          case let .just(borrowing b1):
            return a1 == b1
          case .nothing:
            return false
        }
      case .nothing:
        switch b {
          case .just:
            return false
          case .nothing:
            return true
        }
    }
  }
}


// FIXME: triggers crash!
// @inlinable
// public func fromMaybe<A: ~Copyable>(_ defaultVal: consuming A, 
//                                     _ mayb: consuming Maybe<A>) -> A {
//   switch mayb {
//     case let .just(payload):
//       return payload
//     case .nothing:
//       return defaultVal
//   }
// }

public func isJust<A: ~Copyable>(_ m: borrowing Maybe<A>) -> Bool {
  switch m {
    case .just:
      return true
    case .nothing:
      return false
  }
}

@inlinable
public func isNothing<A: ~Copyable>(_ m: borrowing Maybe<A>) -> Bool {
  return !isJust(m)
}

public struct UnownedRef<Instance: AnyObject> {
  @usableFromInline
  internal unowned(unsafe) var _value: Instance
}
