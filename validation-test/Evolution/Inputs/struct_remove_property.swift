
#if BEFORE

public struct RemoveStoredProperty {
  public init(first: String, middle: String, last: String) {
    self.first = first
    self.middle = middle
    self.last = last
  }

  public var first: String
  public var middle: String
  public var last: String

  public var name: String {
    return "\(first) \(middle) \(last)"
  }
}

#else

public struct RemoveStoredProperty {
  public init(first: String, middle: String, last: String) {
    self.name = "\(first) \(middle) \(last)"
  }

  public var name: String
}

#endif


