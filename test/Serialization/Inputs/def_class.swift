public class Empty {}

public class TwoInts {
  public var x, y : Int
  
  required public init(a : Int, b : Int) {
    x = a
    y = b
  }
}

public class ComputedProperty {
  public var value : Int {
    get {
      var result = 0
      return result
    }
    set(newVal) {
      // completely ignore it!
    }
  }

  public var readOnly : Int {
    return 42
  }

  public init() {}
}


// Generics
public class Pair<A, B> {
  public var first : A
  public var second : B

  public init(a : A, b : B) {
    first = a
    second = b
  }
}

public class GenericCtor<U> {
  public init<T>(_ t : T) {}
  
  public func doSomething<T>(t : T) {}
}


// Protocols
public protocol Resettable {
  func reset()
}

public extension Resettable {
  final func doReset() { self.reset() }
}

public class ResettableIntWrapper : Resettable {
  public var value : Int
  public init() { value = 0 }
  public func reset() {
    var zero = 0
    value = zero
  }
}

public protocol Computable {
  func compute()
}

public typealias Cacheable = protocol<Resettable, Computable>

public protocol SpecialResettable : Resettable, Computable {}

public protocol PairLike {
  typealias FirstType
  typealias SecondType
  func getFirst() -> FirstType
  func getSecond() -> SecondType
}

public protocol ClassProto : class {}

@objc public protocol ObjCProtoWithOptional {
  optional func optionalMethod()
  optional var optionalVar: Int { get }
  optional subscript (i: Int) -> Int { get }
}


public class OptionalImplementer : ObjCProtoWithOptional {
  public func unrelated() {}
  public init() {}
}


// Inheritance
public class StillEmpty : Empty, Resettable {
  public func reset() {}
  public override init() {}
}

public class BoolPair<T> : Pair<Bool, Bool>, PairLike {
  public init() { super.init(a: false, b: false) }
  public func bothTrue() -> Bool {
    return first && second
  }

  public func getFirst() -> Bool { return first }
  public func getSecond() -> Bool { return second }
}

public class SpecialPair<A> : Pair<Int, Int>, Computable {
  public func compute() {}
}

public class OtherPair<A, B> : PairLike {
  public var first : A
  public var second : B

  public init(a : A, b : B) {
    first = a
    second = b
  }

  public typealias FirstType = Bool
  public typealias SecondType = Bool

  public func getFirst() -> Bool { return true }
  public func getSecond() -> Bool { return true }
}

public class OtherBoolPair<T> : OtherPair<Bool, Bool> {
}

public class RequiresPairLike<P : PairLike> { }

public func getReqPairLike() -> RequiresPairLike<OtherBoolPair<Bool>> { 
  return RequiresPairLike<OtherBoolPair<Bool>>()
}


// Subscripts
public class ReadonlySimpleSubscript {
  public subscript(x : Int) -> Bool {
    return true
  }
  public init() {}
}

public class ComplexSubscript {
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


// Destructor
public class Resource {
  public init() { }
  deinit {}
}


// Ownership
public class ResourceSharer {
  // FIXME: Cannot perform in-class initialization here
  public unowned var alwaysPresent : Resource
  public weak var maybePresent : Resource?

  public init (res: Resource) {
    self.alwaysPresent = res
    self.maybePresent = nil
  }
}
