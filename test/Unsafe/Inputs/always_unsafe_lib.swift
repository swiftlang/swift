@unsafe(always)
public func alwaysUnsafeFunc() { }

@unsafe
public func unsafeFunc() { }

@unsafe(always)
public struct AlwaysUnsafeStruct {
  public init() { }
}

// An inlinable body that acknowledges an always-unsafe use. Note that the
// 'unsafe' marker is stripped when this is printed into a module interface, so
// the always-unsafe diagnostic has to be downgraded there.
@inlinable
public func inlinableUseOfAlwaysUnsafe() {
  unsafe alwaysUnsafeFunc()
}
