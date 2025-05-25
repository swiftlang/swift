import _Differentiation

public protocol Protocol {
  var x : Float {get set}
  init()
}

extension Protocol {
  public init(_ val: Float) {
    self.init()
    x = val
  }

  @_alwaysEmitIntoClient
  public func sum() -> Float { x }
}
