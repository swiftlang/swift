public protocol LibP {
  func describe() -> String
}

public struct LibBox<T>: LibP {
  public var t: T
  @inlinable
  public init(t: T) { self.t = t }
  @inlinable
  public func describe() -> String { "LibBox<\(T.self)>(\(t))" }
}

@inline(never)
public func libMakeBox() -> any LibP {
  return LibBox(t: 42)
}

public protocol LibQ: LibP {
  associatedtype A: LibP
  func make() -> A
}

public struct LibWrapper<T>: LibQ {
  public var t: T
  @inlinable
  public init(t: T) { self.t = t }
  @inlinable
  public func describe() -> String { "LibWrapper<\(T.self)>(\(t))" }
  @inlinable
  public func make() -> LibBox<T> { LibBox(t: t) }
}

// A type which erases itself into another protocol.
public protocol LibOuter {
  func erase() -> any LibP
}

public struct LibOuterBox<T>: LibOuter {
  public var t: T
  @inlinable
  public init(t: T) { self.t = t }
  @inlinable
  public func erase() -> any LibP { LibBox(t: t) }
}
