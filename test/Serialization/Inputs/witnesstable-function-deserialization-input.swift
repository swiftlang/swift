
@_silgen_name("evil") public func _evil()

public func id<U>(_ u : U) -> U {
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  return u
}

public protocol Z {
  func f() -> Z
}

public struct X : Z {
  public func f() -> Z {
    return id(self)
  }
  public init() {}
}

public func makeZDoSomething(_ z : Z) -> Z {
  return z.f()
}
