public struct Source: ~Copyable {
  public init() {}

  public var mutableSpan: MutableSpan<Int> {
    @_lifetime(&self)
    mutating get {
      MutableSpan()
    }
  }
}
