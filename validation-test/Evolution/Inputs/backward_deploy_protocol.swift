public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public protocol OtherProtocol {
  init()
}
public struct OtherConcrete : OtherProtocol {
  public init() {}
}

public protocol OldProtocol {
#if AFTER
  @_weakLinked associatedtype NewType: OtherProtocol = OtherConcrete
  @_weakLinked func newMethod() -> NewType
#endif
}

#if AFTER
extension OldProtocol {
  @_weakLinked public func newMethod() -> NewType {
    return NewType()
  }
}
#endif

#if AFTER
@_weakLinked public protocol NewProtocol {
  func newMethod()
}
#endif

public struct NewConforms {
  public init() {}
}

#if AFTER
@_weakLinked extension NewConforms : NewProtocol {
  public func newMethod() {}
}
#endif

