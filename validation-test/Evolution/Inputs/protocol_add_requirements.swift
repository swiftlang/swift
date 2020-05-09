
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}


public protocol ElementProtocol : Equatable {
  func increment() -> Self
}

public protocol AddMethodsProtocol {
  associatedtype Element : ElementProtocol

  func importantOperation() -> Element
  func unimportantOperation() -> Element

#if AFTER
  @_weakLinked func uselessOperation() -> Element
#endif
}

extension AddMethodsProtocol {
  public func unimportantOperation() -> Element {
    return importantOperation().increment()
  }

#if AFTER
  @_weakLinked public func uselessOperation() -> Element {
    return unimportantOperation().increment()
  }
#endif
}

public func doSomething<T : AddMethodsProtocol>(_ t: T) -> [T.Element] {
#if BEFORE
  return [
      t.importantOperation(),
      t.unimportantOperation(),
      t.unimportantOperation().increment(),
  ]
#else
  return [
      t.importantOperation(),
      t.unimportantOperation(),
      t.uselessOperation(),
  ]
#endif
}


public protocol AddConstructorsProtocol {
  init(name: String)

#if AFTER
  @_weakLinked init?(nickname: String)
#endif
}

extension AddConstructorsProtocol {
  @_weakLinked public init?(nickname: String) {
    if nickname == "" {
      return nil
    }
    self.init(name: nickname + "ster")
  }
}

public func testConstructorProtocol<T : AddConstructorsProtocol>(_ t: T.Type) -> [T?] {
#if BEFORE
  return [t.init(name: "Puff")]
#else
  return [t.init(name: "Meow meow"),
          t.init(nickname: ""),
          t.init(nickname: "Robster the Lob")]
#endif
}


public protocol AddPropertiesProtocol {
  var topSpeed: Int { get nonmutating set }
  var maxRPM: Int { get set }

#if AFTER
  @_weakLinked var maxSafeSpeed: Int { get set }
  @_weakLinked var minSafeSpeed: Int { get nonmutating set }
  @_weakLinked var redLine: Int { mutating get set }
#endif
}

extension AddPropertiesProtocol {
#if AFTER
  @_weakLinked public var maxSafeSpeed: Int {
    get {
      return topSpeed / 2
    }
    set {
      topSpeed = newValue * 2
    }
  }

  @_weakLinked public var minSafeSpeed: Int {
    get {
      return topSpeed / 4
    }
    nonmutating set {
      topSpeed = newValue * 4
    }
  }

  @_weakLinked public var redLine: Int {
    get {
      return maxRPM - 2000
    }
    set {
      maxRPM = newValue + 2000
    }
  }
#endif
}

public func getProperties<T : AddPropertiesProtocol>(_ t: inout T) -> [Int] {
#if BEFORE
  return [t.topSpeed, t.maxRPM]
#else
  return [t.topSpeed, t.maxRPM, t.maxSafeSpeed, t.minSafeSpeed, t.redLine]
#endif
}

func increment(_ x: inout Int, by: Int) {
  x += by
}

public func setProperties<T : AddPropertiesProtocol>(_ t: inout T) {
#if AFTER
  t.minSafeSpeed = t.maxSafeSpeed
  increment(&t.redLine, by: 7000)
#else
  increment(&t.topSpeed, by: t.topSpeed)
  increment(&t.maxRPM, by: 7000)
#endif
}


public protocol AddSubscriptProtocol {
  associatedtype Key
  associatedtype Value

  func get(key: Key) -> Value
  mutating func set(key: Key, value: Value)

#if AFTER
  @_weakLinked subscript(key: Key) -> Value { get set }
#endif
}

extension AddSubscriptProtocol {
  @_weakLinked public subscript(key: Key) -> Value {
    get {
      return get(key: key)
    }
    set {
      set(key: key, value: newValue)
    }
  }
}

public func doSomething<T : AddSubscriptProtocol>(_ t: inout T, k1: T.Key, k2: T.Key) {
#if BEFORE
  t.set(key: k1, value: t.get(key: k2))
#else
  t[k1] = t[k2]
#endif
}

public protocol SimpleProtocol {
  static func getString() -> String
}

public struct Wrapper<T>: SimpleProtocol {
  public static func getString() -> String {
    return "I am a wrapper for \(T.self)"
  }
}

public protocol AddAssocTypesProtocol {
#if AFTER
  @_weakLinked associatedtype AssocType = Self
  @_weakLinked associatedtype AssocType2: SimpleProtocol = Wrapper<AssocType>
#endif
}

public func doSomethingWithAssocTypes<T: AddAssocTypesProtocol>(_ value: T)
    -> String {
#if AFTER
  return String(describing: T.AssocType2.self)
#else
  return "there are no associated types yet"
#endif
}

public func doSomethingWithAssocConformances<T: AddAssocTypesProtocol>(_ value: T)
    -> String {
#if AFTER
  let at2: Any.Type = T.AssocType2.self
  if let simpleType = at2 as? SimpleProtocol.Type {
    return simpleType.getString()
  }
  
  return "missing associated conformance"
#else
  return "there are no associated conformances yet"
#endif
}
