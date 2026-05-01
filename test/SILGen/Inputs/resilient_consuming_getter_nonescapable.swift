public struct NCNE: ~Escapable, ~Copyable {
  var _bytes: UnsafeMutableBufferPointer<Int>

  @_owned public var bytes: MutableSpan<Int> {
    @_lifetime(copy self)
    consuming get {
      _overrideLifetime(MutableSpan(_unsafeElements: _bytes), copying: self)
    }
  }
}

public struct NC: ~Copyable {
  public init() {}

  @_owned public var value: NC {
    consuming get {
      NC()
    }
  }
}
