
public func foo(x: Int = 0) -> Int { x }

public struct Subscript1 {
  public init() { }
  public subscript(x: Int = 0) -> Int { x }
}

public struct Subscript2 {
  public init() { }
  public subscript(x: String = #function) -> String { x }
}
