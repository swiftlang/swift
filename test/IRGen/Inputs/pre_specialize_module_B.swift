import A

public class AnotherThing {
  public init() {}
}

@_specializeExtension
extension InternalThing {

  @_specialize(exported: true, target: compute() , where T == AnotherThing)
  @_specialize(exported: true, target: compute(), where T == ResilientInternalBoxedThing<AnotherThing>)
  public func specializeCompute() -> T {
    fatalError("don't call")
  }

  public var specializeComputedX : T {
    @_specialize(exported: true, target: computedX, where T == AnotherThing)
    get {
      fatalError("don't call")
    }
    @_specialize(exported: true, target: computedX, where T == AnotherThing)
    set {
      fatalError("don't call")
    }
  }

  public subscript(specialized i: Int) -> T {
    @_specialize(exported: true, target: subscript(_:), where T == AnotherThing)
    get {
      fatalError("don't call")
    }
    @_specialize(exported: true, target: subscript(_:), where T == AnotherThing)
    set {
      fatalError("don't call")
    }
  }
}

@_specializeExtension
extension InternalRef {

  @_specialize(exported: true, target: compute() , where T == AnotherThing)
  public func specializeCompute() -> T {
    fatalError("don't call")
  }

  public var specializeComputedX : T {
    @_specialize(exported: true, target: computedX, where T == AnotherThing)
    get {
      fatalError("don't call")
    }
    @_specialize(exported: true, target: computedX, where T == AnotherThing)
    set {
      fatalError("don't call")
    }
  }

  public subscript(specialized i: Int) -> T {
    @_specialize(exported: true, target: subscript(_:), where T == AnotherThing)
    get {
      fatalError("don't call")
    }
    @_specialize(exported: true, target: subscript(_:), where T == AnotherThing)
    set {
      fatalError("don't call")
    }
  }
}
