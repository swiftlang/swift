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

#if AFTER
// Protocol is always available, type is weak-linked
@_weakLinked public struct OtherConforms : OtherProtocol {
  public init() {}
}
#endif

public protocol OldProtocol {
#if AFTER
  @_weakLinked associatedtype NewType: OtherProtocol = OtherConforms
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

// Protocol is weak-linked, type is always available
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

// Protocol and type are always available, conformace is weak-linked
public struct NewConformanceConforms {
  public init() {}
}
public protocol NewConformanceProtocol {}

#if AFTER
@_weakLinked extension NewConformanceConforms : NewConformanceProtocol {}
#endif
