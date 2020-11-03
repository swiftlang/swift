@_specialize(exported: true, where T == Int)
@_specialize(exported: true, where T == Double)
public func publicPrespecialized<T>(_ t: T) {
}

@_specialize(exported: true, where T == Int)
@_specialize(exported: true, where T == Double)
@_alwaysEmitIntoClient
internal func internalEmitIntoClientPrespecialized<T>(_ t: T) {
}

@inlinable
public func useInternalEmitIntoClientPrespecialized<T>(_ t: T) {
  internalEmitIntoClientPrespecialized(t)
}

@inlinable
public func publicInlineable<T>(_ t: T) {
}

public struct ResilientThing {
  public init() {}
}

@usableFromInline
@frozen
internal struct InternalThing2<T> {
  @usableFromInline
  var x : T

  @usableFromInline
  init(_ t: T) {
    x = t
  }

  @_specialize(exported: true, where T == Int)
  @inlinable
  func compute() -> T {
    return x
  }

  @inlinable
  var computedX : T {
    @_specialize(exported: true, where T == Int)
    get {
    return x
    }
  }

  @inlinable
  var computedY : T {
    @_specialize(exported: true, where T == Int)
    get {
      return x
    }
    @_specialize(exported: true, where T == Int)
    set {
      x = newValue
    }
  }

  @inlinable
  var computedZ : T {
    @_specialize(exported: true, where T == Int)
    _modify {
      yield &x
    }
    @_specialize(exported: true, where T == Int)
    _read {
      yield x
    }
  }
  @inlinable
  subscript(_ i: Int) -> T {
    @_specialize(exported: true, where T == Int)
    get {
      return x
    }
    @_specialize(exported: true, where T == Int)
    set {
    }
  }
}

@inlinable
public func useInternalThing<T>(_ t: T) {
  var x = InternalThing2(t)
  print(x.compute())
  print(x.computedX)
  x.computedY = t
  print(x.computedY)
  x.computedZ = t
  print(x.computedZ)
  x[1] = t
  print(x[1])
}
