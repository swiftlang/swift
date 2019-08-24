open class Empty {}

open class TwoInts {
  open var x, y : Int
  
  required public init(a : Int, b : Int) {
    x = a
    y = b
  }
}

open class ComputedProperty {
  open var value : Int {
    get {
      var result = 0
      return result
    }
    set(newVal) {
      // completely ignore it!
    }
  }

  open var readOnly : Int {
    return 42
  }

  public init() {}
}


// Generics
open class Pair<A, B> {
  open var first : A
  open var second : B

  public init(a : A, b : B) {
    first = a
    second = b
  }
}

open class GenericCtor<U> {
  public init<T>(_ t : T) {}
  
  open func doSomething<T>(_ t : T) {}
}


// Protocols
public protocol Resettable {
  func reset()
}

public extension Resettable {
  func doReset() { self.reset() }
}

open class ResettableIntWrapper : Resettable {
  open var value : Int
  public init() { value = 0 }
  open func reset() {
    var zero = 0
    value = zero
  }
}

public protocol Computable {
  func compute()
}

public typealias Cacheable = Resettable & Computable

public protocol SpecialResettable : Resettable, Computable {}

public protocol PairLike {
  associatedtype FirstType
  associatedtype SecondType
  func getFirst() -> FirstType
  func getSecond() -> SecondType
}

public extension PairLike where FirstType : Cacheable {
  func cacheFirst() { }
}

public protocol ClassProto : class {}

@objc public protocol ObjCProtoWithOptional {
  @objc optional func optionalMethod()
  @objc optional var optionalVar: Int { get }
  @objc optional subscript (i: Int) -> Int { get }
}


open class OptionalImplementer : ObjCProtoWithOptional {
  open func unrelated() {}
  public init() {}
}


// Inheritance
open class StillEmpty : Empty, Resettable {
  open func reset() {}
  public override init() {}
}

open class BoolPair<T> : Pair<Bool, Bool>, PairLike {
  public init() { super.init(a: false, b: false) }
  open func bothTrue() -> Bool {
    return first && second
  }

  open func getFirst() -> Bool { return first }
  open func getSecond() -> Bool { return second }
}

open class SpecialPair<A> : Pair<Int, Int>, Computable {
  open func compute() {}
}

open class OtherPair<A, B> : PairLike {
  open var first : A
  open var second : B

  public init(a : A, b : B) {
    first = a
    second = b
  }

  public typealias FirstType = Bool
  public typealias SecondType = Bool

  open func getFirst() -> Bool { return true }
  open func getSecond() -> Bool { return true }
}

open class OtherBoolPair<T> : OtherPair<Bool, Bool> {
}

open class RequiresPairLike<P : PairLike> { }

public func getReqPairLike() -> RequiresPairLike<OtherBoolPair<Bool>> { 
  return RequiresPairLike<OtherBoolPair<Bool>>()
}


// Subscripts
open class ReadonlySimpleSubscript {
  open subscript(x : Int) -> Bool {
    return true
  }
  public init() {}
}

open class ComplexSubscript {
  open subscript(x : Int, y : Bool) -> Int {
    set(newValue) {
      // do nothing!
    }
    get {
      return 0
    }
  }
  public init() {}
}


// Destructor
open class Resource {
  public init() { }
  deinit {}
}


// Ownership
open class ResourceSharer {
  // FIXME: Cannot perform in-class initialization here
  open unowned var alwaysPresent : Resource
  open weak var maybePresent : Resource?

  public init (res: Resource) {
    self.alwaysPresent = res
    self.maybePresent = nil
  }
}
