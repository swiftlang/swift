public class Klass {
  public init() {}
}

public struct NonTrivial {
  public var k = Klass()

  public init() {}

  public mutating func mutateInPlace() {}
}

public struct Wrapper {
  var _value: NonTrivial

  public init(_ value: NonTrivial) { _value = value }

  public var value: NonTrivial {
    borrow { return _value }
    mutate { return &_value }
  }
}

public struct NC: ~Copyable {
  var x: Int = 0

  public init() {}

  public mutating func mutateInPlace() {}
}

public struct NCWrapper: ~Copyable {
  var _value: NC

  public init(_ value: consuming NC) { _value = value }

  public var value: NC {
    borrow { return _value }
    mutate { return &_value }
  }
}
