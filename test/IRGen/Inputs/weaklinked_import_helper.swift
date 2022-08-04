public func fn() {}

public var globalStored = 0

public var globalComputed: Int {
  get { return 1 }
  set {}
}

public struct S {
  public func fn() {}

  public var storedProp = 0

  public var computedProp: Int {
    get { return 1 }
    set {}
  }

  public init() {}

  public subscript(_: Int) -> Int {
    get { return 1 }
    set {}
  }
}

public enum E {
  case basic
  case assoc(Int)
}

open class C {
  open func fn() {}

  open var storedProp = 0

  open var computedProp: Int {
    get { return 1 }
    set {}
  }

  public init() {}

  open subscript(_: Int) -> Int {
    get { return 1 }
    set {}
  }
}

public protocol P {
  func fn()

  var prop: Int { get set }

  init()

  subscript(_: Int) -> Int { get set }
}

public struct GenericS<T> {}

public enum GenericE<T> {}

open class GenericC<T> {
  public init() {}
}

public protocol OtherProtocol {}
public struct ConcreteType: OtherProtocol {}

public protocol ProtocolWithAssoc {
  associatedtype T: OtherProtocol = ConcreteType
  func f()
}

extension ProtocolWithAssoc {
  public func f() {}
}

public protocol BaseP {}
extension S: BaseP {}
