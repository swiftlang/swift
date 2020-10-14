import A

extension PublicThing {
  @_specialize(exported: true, kind: full, target: doStuffWith(_:), where T == Int)
  public func specializedoStuff2(_ t: T) {}
}

@_specializeExtension
extension InternalThing {
  @_specialize(exported: true, target:doStuffWith(boxed:), where T == Int)
  public func specializedDoStuffWith2(_ t: BoxedThing<T>) {}
}

@_specializeExtension
extension InternalThing2 {
  public var specializeComputedZ : T {
    @_specialize(exported: true, target: computedZ, where T == Int)
    _modify {
      fatalError("don't call")
    }
    @_specialize(exported: true, target: computedZ, where T == Int)
    _read {
      fatalError("don't call")
    }
  }

  public subscript(specialized i: Int) -> T {
    @_specialize(exported: true, target: subscript(_:), where T == Int)
    get {
      fatalError("don't call")
    }
    @_specialize(exported: true, target: subscript(_:), where T == Int)
    set {
      fatalError("don't call")
    }
  }
}
