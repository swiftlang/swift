// RUN: %target-typecheck-verify-swift

struct Foo<T> {
  var maybeT: T? { return nil }
}

extension Foo {
  struct Bar<U, V> {
    var maybeT: T? { return nil }
    var maybeU: U? { return nil }
    var maybeV: V? { return nil }

    struct Inner {
      var maybeT: T? { return nil }
      var maybeU: U? { return nil }
      var maybeV: V? { return nil }
    }
  }
}

typealias FooInt = Foo<Int>

extension FooInt {
  func goodT() -> Int {
    return maybeT!
  }

  func badT() -> Float {
    return maybeT! // expected-error{{cannot convert return expression of type 'Int' to return type 'Float'}}
  }
}

typealias FooIntBarFloatDouble = Foo<Int>.Bar<Float, Double>

extension FooIntBarFloatDouble {
  func goodT() -> Int {
    return maybeT!
  }
  func goodU() -> Float {
    return maybeU!
  }
  func goodV() -> Double {
    return maybeV!
  }

  func badT() -> Float {
    return maybeT! // expected-error{{cannot convert return expression of type 'Int' to return type 'Float'}}
  }
  func badU() -> Int {
    return maybeU! // expected-error{{cannot convert return expression of type 'Float' to return type 'Int'}}
  }
  func badV() -> Int {
    return maybeV! // expected-error{{cannot convert return expression of type 'Double' to return type 'Int'}}
  }
}

typealias FooIntBarFloatDoubleInner = Foo<Int>.Bar<Float, Double>.Inner

extension FooIntBarFloatDoubleInner {
  func goodT() -> Int {
    return maybeT!
  }
  func goodU() -> Float {
    return maybeU!
  }
  func goodV() -> Double {
    return maybeV!
  }

  func badT() -> Float {
    return maybeT! // expected-error{{cannot convert return expression of type 'Int' to return type 'Float'}}
  }
  func badU() -> Int {
    return maybeU! // expected-error{{cannot convert return expression of type 'Float' to return type 'Int'}}
  }
  func badV() -> Int {
    return maybeV! // expected-error{{cannot convert return expression of type 'Double' to return type 'Int'}}
  }
}

struct Foo2<T1, T2, T3,> {}

typealias Bar2<
  T1,
  T2,
> = Foo2<
  T1,
  T2,
  Bool,
>

let _ = Foo2<Int, Bool, String,>.self
let _ = Bar2<Int, Bool,>()
