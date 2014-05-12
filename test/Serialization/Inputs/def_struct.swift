struct Empty {}

struct TwoInts {
  var x, y : Int
}

struct ComputedProperty {
  var value : Int {
    get {
      var result = 0
      return result
    }
  }
}

// Generics
struct Pair<A, B> {
  var first : A
  var second : B

  init(a : A, b : B) {
    first = a
    second = b
  }
}

typealias VoidPairTuple = ((), ())

struct GenericCtor<U> {
  init<T>(_ t : T) {}

  func doSomething<T>(t: T) {}

  @conversion func __conversion() -> VoidPairTuple {
    return ((), ())
  }
}

// Protocols
protocol Resettable {
  mutating
  func reset()
}

struct ResettableIntWrapper : Resettable {
  var value : Int
  mutating
  func reset() {
    var zero = 0
    value = zero
  }
}

protocol Computable {
  mutating
  func compute()
}

typealias Cacheable = protocol<Resettable, Computable>

protocol SpecialResettable : Resettable, Computable {}

protocol HasAssociatedType {
  typealias ComputableType : Computable
}

struct ComputableWrapper<T : Computable> : HasAssociatedType {
  typealias ComputableType = T
}

protocol AnotherAssociatedType {
  typealias ResettableType : Resettable
}

struct ResettableWrapper<T : Resettable> : AnotherAssociatedType {
  typealias ResettableType = T
}

func cacheViaWrappers<
  T : HasAssociatedType, U : AnotherAssociatedType
    where T.ComputableType == U.ResettableType
>(computable : T, resettable : U) {}


// Subscripts
struct ReadonlySimpleSubscript {
  subscript(x : Int) -> Bool {
    return true
  }
}

struct ComplexSubscript {
  subscript(x : Int, y : Bool) -> Int {
    set(newValue) {
      // do nothing!
    }
    get {
      return 0
    }
  }
}


// Extensions
extension Empty {
  func doAbsolutelyNothing() {}
}

struct UnComputable {}
extension UnComputable : Computable {
  init(x : Int) {}
  func compute() {}
  static func canCompute() -> Bool {
    return true
  }
}

extension Pair {
  func swap() -> (B, A) {
    return (second, first)
  }
}
