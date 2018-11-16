
#if BEFORE

public struct RemoveStoredProperty {
  public init(first: String, middle: String, last: String) {
    self.first = first
    self.middle = middle
    self.last = last
  }

  var first: String
  var middle: String
  var last: String

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


