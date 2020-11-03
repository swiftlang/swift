@frozen
public struct PublicThing<T> {
  @_specialize(exported: true, where T == Double)
  @inlinable
  public func doStuffWith(_ t: T) {
    print(t)
  }
  public init(_ t: T) {}
}

@frozen
public struct PublicPlainThing {
  public init() {}
}

@usableFromInline
@frozen
internal struct BoxedThing<T> {}

@usableFromInline
@frozen
internal struct BoxedThing2<T> {
  var t: T

  init(_ t: T) {
    self.t = t
  }
}


@usableFromInline
@frozen
internal struct InternalThing<T> {
  @inlinable
  func doStuffWith(_ t: T) {
    print(t)
  }

  @_specialize(exported: true, where T == Double)
  @inlinable
  func doStuffWith(boxed: BoxedThing<T>) {
    print(boxed)
  }

  @inlinable
  func doStuffWith(boxed2: BoxedThing2<T>) {
    print(boxed2)
  }

  @usableFromInline
  init(_ t: T) {}
}

public struct ResilientThing {
  public init() {}
}

@usableFromInline
@frozen
internal struct InternalThing2<T> {
  @usableFromInline
  var x : T

  init(_ t: T) {
    x = t
  }

  @inlinable
  var computedX : T {
    return x
  }

  @inlinable
  var computedY : T {
    get {
      return x
    }
    set {
      x = newValue
    }
  }

  @inlinable
  var computedZ : T {
    _modify {
      yield &x
    }
    _read {
      yield x
    }
  }

  @inlinable
  subscript(_ i: Int) -> T {
    get {
      return x
    }
    set {
      x = newValue
    }
  }
}
