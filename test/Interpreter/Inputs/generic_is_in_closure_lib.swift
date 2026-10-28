public protocol P: ~Copyable {}
public struct Box<T>: P, ~Copyable {
  public var t: T
  public init(_ t: T) { self.t = t }
}

public func run(_ x: borrowing any P & ~Copyable,
                _ matches: (borrowing any P & ~Copyable) -> Bool) -> Bool {
  matches(x)
}

public func check<E>(_ x: borrowing any P & ~Copyable, _ t: E.Type) -> Bool {
  run(x, { $0 is Box<E> })
}
