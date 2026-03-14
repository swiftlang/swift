@_weakLinked
public func fn() {}

@_weakLinked
public var globalStored = 0

@_weakLinked
public var globalComputed: Int {
  get { return 1 }
  set {}
}

public struct S {
  @_weakLinked
  public func fn() {}

  @_weakLinked
  public var storedProp = 0

  @_weakLinked
  public var computedProp: Int {
    get { return 1 }
    set {}
  }

  @_weakLinked
  public init() {}

  @_weakLinked
  public subscript(_: Int) -> Int {
    get { return 1 }
    set {}
  }
}

extension S {
  @_weakLinked
  public func extensionFn() {}

  @_weakLinked
  public var extensionProp: Int {
    get { return 1 }
    set {}
  }
}

public enum E {
  case strong

  @_weakLinked
  case weak

  case strongAssoc(Int)

  @_weakLinked
  case weakAssoc(Int)
}

open class C {
  @_weakLinked
  open func fn() {}

  @_weakLinked
  open var storedProp = 0

  @_weakLinked
  open var computedProp: Int {
    get { return 1 }
    set {}
  }

  @_weakLinked
  public init() {}

  @_weakLinked
  open subscript(_: Int) -> Int {
    get { return 1 }
    set {}
  }
}

public protocol P {
  @_weakLinked
  func fn()

  @_weakLinked
  var prop: Int { get set }

  @_weakLinked
  init()

  @_weakLinked
  subscript(_: Int) -> Int { get set }
}

@_weakLinked
public struct WeakS {
  public init() {}
  public func weakMember() {}
}

@_weakLinked
public enum WeakE {}

@_weakLinked
open class WeakC {
  public init() {}
}

@_weakLinked
public protocol WeakP {}


@_weakLinked
public struct GenericS<T> {}

@_weakLinked
public enum GenericE<T> {}

@_weakLinked
open class GenericC<T> {
  public init() {}
}

public protocol OtherProtocol {}
public struct ConcreteType : OtherProtocol {}

public protocol ProtocolWithWeakMembers {
  @_weakLinked associatedtype T : OtherProtocol = ConcreteType
  @_weakLinked func f()
}

extension ProtocolWithWeakMembers {
  @_weakLinked public func f() {}
}

public protocol BaseP {}
@_weakLinked extension S : BaseP {}
