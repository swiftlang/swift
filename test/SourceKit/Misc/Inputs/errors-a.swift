this file has a _bunch_ of errors, but we
want to generate the module anyway!

public typealias InvalidAlias = undefined

public class InvalidClass: undefined, InvalidProtocol {
  public var classMemberA: undefined

  public init(param1: undefined, param2: undefined) {
    undefined
  }

  public convenience init() {
    self.init(undefined, undefined)
  }

  public convenience init(param: undefined) {}
}

public class InvalidClassSub1: InvalidClass {
  public var classMemberB: undefined

  public override init(param1: undefined, param2: undefined) {
    undefined
  }

  public convenience init() {}
}

public class InvalidClassSub2: InvalidClass {
  public var classMemberC: undefined

  public convenience init() {}
}

public enum InvalidEnum {
  var badEnumMemberA

  case enumeratorA
  case enumeratorB, enumeratorC trailing
}

public struct InvalidGenericStruct<T: InvalidProtocol, U: undefined>
    where T.Item == U.Item, T.Item: undefined {
  public var genericMemberA: undefined
}

public protocol InvalidProtocol : undefined {
  associatedtype Item
  mutating func add(_)
  func get() -> Item
  mutating func set(item: Item)
}

public struct InvalidStruct : undefined, InvalidProtocol {
  typealias Item = undefined

  public let memberA: Int {
    willSet(newVal invalid) {
      print("Setting value \(newVal)")
    }
    didSet(oldVal invalid) {
      print("Set value \(oldVal)")
    }
  }
  public let memberB: undefined {
    willSet {
      print("Setting value \(newValue)")
    }
    didSet {
      print("Set value \(oldValue)")
    }
  }
  public var memberC: undefined = {
    return undefined
  }()
  public lazy var memberD: undefined = {
    return undefined
  }()
  public var memberE: undefined {
    get { undefined }
    set { undefined = "bar" }
  }

  mutating func set(item: Item) {
    undefined
  }
}

public extension undefined {
  public enum ValidEnum: String {
    case a
    case b
    case c
  }
}
