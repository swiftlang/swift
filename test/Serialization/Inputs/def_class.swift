class Empty {}

class TwoInts {
  var x, y : Int
  
  constructor(a : Int, b : Int) {
    x = a
    y = b
  }
}

class ComputedProperty {
  var value : Int {
  get:
    var result : Int
    return result
  }
}

// Generics
class Pair<A, B> {
  var first : A
  var second : B

  constructor(a : A, b : B) {
    first = a
    second = b
  }
}

class GenericCtor<U> {
  constructor<T>(t : T) {}
  
  func doSomething<T>(t : T) {}
}

// Protocols
protocol Resettable {
  func reset()
}

class ResettableIntWrapper : Resettable {
  var value : Int
  func reset() {
    var zero : Int
    value = zero
  }
}

protocol Computable {
  func compute()
}

typealias Cacheable = protocol<Resettable, Computable>

protocol SpecialResettable : Resettable, Computable {}


// Inheritance

class StillEmpty : Empty, Resettable {
  func reset() {}
}

class BoolPair : Pair<Bool, Bool> {
  func bothTrue() -> Bool {
    return first && second
  }
}

class SpecialPair<A> : Computable, Pair<Int, Int> {
  func compute() {}
}
