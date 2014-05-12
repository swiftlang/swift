class Empty {}

class TwoInts {
  var x, y : Int
  
  @required init(a : Int, b : Int) {
    x = a
    y = b
  }
}

class ComputedProperty {
  var value : Int {
    get {
      var result = 0
      return result
    }
    set(newVal) {
      // completely ignore it!
    }
  }

  var readOnly : Int {
    return 42
  }
}


// Generics
class Pair<A, B> {
  var first : A
  var second : B

  init(a : A, b : B) {
    first = a
    second = b
  }
}

class GenericCtor<U> {
  init<T>(_ t : T) {}
  
  func doSomething<T>(t : T) {}
}


// Protocols
protocol Resettable {
  func reset()
}

class ResettableIntWrapper : Resettable {
  var value : Int
  init() { value = 0 }
  func reset() {
    var zero = 0
    value = zero
  }
}

protocol Computable {
  func compute()
}

typealias Cacheable = protocol<Resettable, Computable>

protocol SpecialResettable : Resettable, Computable {}

protocol PairLike {
  typealias FirstType
  typealias SecondType
  func getFirst() -> FirstType
  func getSecond() -> SecondType
}

@class_protocol protocol ClassProto {}

@class_protocol @objc protocol ObjCProtoWithOptional {
  @optional func optionalMethod()
  @optional var optionalVar: Int { get }
  @optional subscript (i: Int) -> Int { get }
}


class OptionalImplementer : ObjCProtoWithOptional {
  func unrelated() {}
}


// Inheritance
class StillEmpty : Empty, Resettable {
  func reset() {}
}

class BoolPair<T> : Pair<Bool, Bool>, PairLike {
  init() { super.init(a: false, b: false) }
  func bothTrue() -> Bool {
    return first && second
  }

  func getFirst() -> Bool { return first }
  func getSecond() -> Bool { return second }
}

class SpecialPair<A> : Pair<Int, Int>, Computable {
  func compute() {}
}

class OtherPair<A, B> : PairLike {
  var first : A
  var second : B

  init(a : A, b : B) {
    first = a
    second = b
  }

  typealias FirstType = Bool
  typealias SecondType = Bool

  func getFirst() -> Bool { return true }
  func getSecond() -> Bool { return true }
}

class OtherBoolPair<T> : OtherPair<Bool, Bool> {
}

class RequiresPairLike<P : PairLike> { }

func getReqPairLike() -> RequiresPairLike<OtherBoolPair<Bool>> { 
  return RequiresPairLike<OtherBoolPair<Bool>>()
}


// Subscripts
class ReadonlySimpleSubscript {
  subscript(x : Int) -> Bool {
    return true
  }
}

class ComplexSubscript {
  subscript(x : Int, y : Bool) -> Int {
    set(newValue) {
      // do nothing!
    }
    get {
      return 0
    }
  }
}


// Destructor
class Resource {
  init() { }
  deinit {}
}


// Ownership
class ResourceSharer {
  // FIXME: Cannot perform in-class initialization here
  unowned var alwaysPresent : Resource
  weak var maybePresent : Resource?

  init (res: Resource) {
    self.alwaysPresent = res
    self.maybePresent = nil
  }
}
