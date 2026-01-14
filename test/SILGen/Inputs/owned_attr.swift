public struct Source: ~Copyable, Giver {
  public init() {}

  @_owned
  public var mutableSpan: MutableSpan<Int> {
    @_lifetime(&self)
    mutating get {
      MutableSpan()
    }
  }
}

public protocol Giver: ~Copyable {
  @_owned
  var mutableSpan: MutableSpan<Int> { mutating get }
}
