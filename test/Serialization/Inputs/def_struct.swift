public struct Empty {}

public struct TwoInts {
  public var x, y : Int
  public init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }
}

public struct ComputedProperty {
  public var value : Int {
    get {
      var result = 0
      return result
    }
  }
}

public struct StaticProperties {
  public static var foo: Int = 0
  public static let bar: Int = 0
  public static var baz: Int {
    return 0
  }
}

// Generics
public struct Pair<A, B> {
  public var first : A
  public var second : B

  public init(a : A, b : B) {
    first = a
    second = b
  }
}

public typealias VoidPairTuple = ((), ())

public struct GenericCtor<U> {
  public init<T>(_ t : T) {}

  public func doSomething<T>(t: T) {}
}

// Protocols
public protocol Resettable {
  mutating
  func reset()
}

public struct ResettableIntWrapper : Resettable {
  public var value : Int
  public mutating
  func reset() {
    var zero = 0
    value = zero
  }
  public init(value: Int) { self.value = value }
}

public protocol Computable {
  mutating
  func compute()
}

public typealias Cacheable = protocol<Resettable, Computable>

public protocol SpecialResettable : Resettable, Computable {}

public protocol HasAssociatedType {
  typealias ComputableType : Computable
}

public struct ComputableWrapper<T : Computable> : HasAssociatedType {
  public typealias ComputableType = T
  public init() {}
}

public protocol AnotherAssociatedType {
  typealias ResettableType : Resettable
}

public struct ResettableWrapper<T : Resettable> : AnotherAssociatedType {
  public typealias ResettableType = T
  public init() {}
}

public func cacheViaWrappers<
  T : HasAssociatedType, U : AnotherAssociatedType
    where T.ComputableType == U.ResettableType
>(computable : T, _ resettable : U) {}


// Subscripts
public struct ReadonlySimpleSubscript {
  public subscript(x : Int) -> Bool {
    return true
  }
  public init() {}
}

public struct ComplexSubscript {
  public subscript(x : Int, y : Bool) -> Int {
    set(newValue) {
      // do nothing!
    }
    get {
      return 0
    }
  }
  public init() {}
}


// Extensions
public extension Empty {
  public func doAbsolutelyNothing() {}
}

public struct UnComputable {}
extension UnComputable : Computable {
  public init(x : Int) {}
  public func compute() {}
  public static func canCompute() -> Bool {
    return true
  }
}

public extension Pair {
  public func swap() -> (B, A) {
    return (second, first)
  }
}
