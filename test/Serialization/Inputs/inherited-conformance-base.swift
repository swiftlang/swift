import Lib

// Adopt SimpleProto via ComplexProto.
public struct Counter<T> : ComplexProto {
  public var value = 0
  
  public func predecessor() -> Counter {
    return Counter(value: value - 1)
  }

  public func successor() -> Counter {
    return Counter(value: value + 1)
  }

  public init(value: Int) { self.value = value }
}
