// Adopt ForwardIndex via BidirectionalIndex.
public struct Counter<T: protocol<RandomAccessIndex, IntegerLiteralConvertible>> : BidirectionalIndex {
  public var value = 0
  
  public func predecessor() -> Counter {
    return Counter(value: value - 1)
  }

  public func successor() -> Counter {
    return Counter(value: value + 1)
  }

  public init(value: Int) { self.value = value }
}

public func == <T>(lhs: Counter<T>, rhs: Counter<T>) -> Bool {
  return lhs.value == rhs.value
}

