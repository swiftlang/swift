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
