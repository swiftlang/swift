// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/keypath.swift -primary-file %s

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // no error for Swift 3/4

    S()[keyPath: \.i] = 1
    // expected-error@-1 {{cannot assign through subscript: function call returns immutable value}}
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // no error for Swift 3/4

  C()[keyPath: \.i] = 1   // warning on write with literal keypath
  // expected-warning@-1 {{forming a writable keypath to property}}

  let _ = C()[keyPath: \.i] // no warning for a read
}

// SR-7339
class Some<T, V> { // expected-note {{'V' declared as parameter to type 'Some'}}
  init(keyPath: KeyPath<T, ((V) -> Void)?>) {
  }
}

class Demo {
  var here: (() -> Void)?
}

let some = Some(keyPath: \Demo.here)
// expected-error@-1 {{cannot convert value of type 'KeyPath<Demo, (() -> Void)?>' to expected argument type 'KeyPath<Demo, ((V) -> Void)?>'}}
// expected-note@-2 {{arguments to generic parameter 'Value' ('(() -> Void)?' and '((V) -> Void)?') are expected to be equal}}
// expected-error@-3 {{generic parameter 'V' could not be inferred}}
// expected-note@-4 {{explicitly specify the generic arguments to fix this issue}}

// SE-0249
func testFunc() {
  let _: (S) -> Int = \.i
  _ = ([S]()).map(\.i)

  // FIXME: A terrible error, but the same as the pre-existing key path
  // error in the similar situation: 'let _ = \S.init'.
  _ = ([S]()).map(\.init)
  // expected-error@-1 {{type of expression is ambiguous without more context}}

  let kp = \S.i
  let _: KeyPath<S, Int> = kp // works, because type defaults to KeyPath nominal
  let f = \S.i
  let _: (S) -> Int = f // expected-error {{cannot convert value of type 'KeyPath<S, Int>' to specified type '(S) -> Int'}}
}


// SR-11234
public extension Array {
    func sorted<C: Comparable, K: KeyPath<Element, C>>(by keyPath: K) -> Array<Element> {
        let sortedA = self.sorted(by: { $0[keyPath: keyPath] < $1[keyPath: keyPath] })
        return sortedA
    }
}

// rdar://problem/54322807
struct X<T> {
  init(foo: KeyPath<T, Bool>) { }
  init(foo: KeyPath<T, Bool?>) { }
}

struct Wibble {
  var boolProperty = false
}

struct Bar {
  var optWibble: Wibble? = nil
}

class Foo {
  var optBar: Bar? = nil
}

func testFoo<T: Foo>(_: T) {
  let _: X<T> = .init(foo: \.optBar!.optWibble?.boolProperty)
}

// rdar://problem/56131416

enum Rdar56131416 {
  struct Pass<T> {}
  static func f<T, U>(_ value: T, _ prop: KeyPath<T, U>) -> Pass<U> { fatalError() }

  struct Fail<T> {}
  static func f<T, U>(_ value: T, _ transform: (T) -> U) -> Fail<U> { fatalError() }
  
  static func takesCorrectType(_: Pass<UInt>) {}
}

func rdar56131416() {
  // This call should not be ambiguous.
  let result = Rdar56131416.f(1, \.magnitude) // no-error
  
  // This type should be selected correctly.
  Rdar56131416.takesCorrectType(result)
}

func test_mismatch_with_contextual_optional_result() {
  struct A<T> {
    init<U: Collection>(_ data: T, keyPath: KeyPath<T, U?>) {}
  }

  struct B {
    var arr: [Int] = []
  }

  let _ = A(B(), keyPath: \.arr)
  // expected-error@-1 {{key path value type '[Int]' cannot be converted to contextual type '[Int]?'}}
}
