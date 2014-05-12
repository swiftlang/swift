// Adopt ForwardIndex via BidirectionalIndex.
struct Counter<T: protocol<RandomAccessIndex, IntegerLiteralConvertible>> : BidirectionalIndex {
  var value = 0
  
  func pred() -> Counter {
    return Counter(value: value - 1)
  }

  func succ() -> Counter {
    return Counter(value: value + 1)
  }
}

func == <T>(lhs: Counter<T>, rhs: Counter<T>) -> Bool {
  return lhs.value == rhs.value
}

