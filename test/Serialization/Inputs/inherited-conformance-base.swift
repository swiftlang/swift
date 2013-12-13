// Adopt ForwardIndex via BidirectionalIndex.
struct Counter<T: protocol<RandomAccessIndex, IntegerLiteralConvertible>> : BidirectionalIndex {
  var value = 0
  
  func __equal__(rhs: Counter) -> Bool {
    return value == rhs.value
  }

  func pred() -> Counter {
    return Counter(value - 1)
  }

  func succ() -> Counter {
    return Counter(value + 1)
  }
}
