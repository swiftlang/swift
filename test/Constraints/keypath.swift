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
  _ = \S.init // expected-error {{key path cannot refer to initializer 'init()'}}
  _ = ([S]()).map(\.init) // expected-error {{key path cannot refer to initializer 'init()'}}

  let kp = \S.i
  let _: KeyPath<S, Int> = kp // works, because type defaults to KeyPath nominal
  let f = \S.i
  let _: (S) -> Int = f // expected-error {{cannot convert value of type 'KeyPath<S, Int>' to specified type '(S) -> Int'}}
}

struct SR_12432 {
  static func takesKeyPath(_: KeyPath<SR_12432.S, String>) -> String { "" }

  struct S {
    let text: String = takesKeyPath(\.text) // okay
  }
}

// SR-11234
public extension Array {
    func sorted<C: Comparable, K: KeyPath<Element, C>>(by keyPath: K) -> Array<Element> {
        let sortedA = self.sorted(by: { $0[keyPath: keyPath] < $1[keyPath: keyPath] })
        return sortedA
    }

  var i: Int { 0 }
}

func takesVariadicFnWithGenericRet<T>(_ fn: (S...) -> T) {}

// rdar://problem/59445486
func testVariadicKeypathAsFunc() {
  // These are okay, the base type of the KeyPath is inferred to be [S].
  let _: (S...) -> Int = \.i
  let _: (S...) -> Int = \Array.i
  takesVariadicFnWithGenericRet(\.i)
  takesVariadicFnWithGenericRet(\Array.i)

  // These are not okay, the KeyPath should have a base that matches the
  // internal parameter type of the function, i.e [S].
  let _: (S...) -> Int = \S.i // expected-error {{key path value type 'S' cannot be converted to contextual type '[S]'}}
  takesVariadicFnWithGenericRet(\S.i) // expected-error {{key path value type 'S' cannot be converted to contextual type '[S]'}}
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

// SR-11184
class SR11184 {}

func fSR11184(_ c: SR11184!, _ kp: ReferenceWritableKeyPath<SR11184, String?>, _ str: String) {
  c[keyPath: kp] = str // OK
  c![keyPath: kp] = str // OK
  c?[keyPath: kp] = str // OK
}

func fSR11184_O(_ c: SR11184!, _ kp: ReferenceWritableKeyPath<SR11184, String?>, _ str: String?) {
  c[keyPath: kp] = str // OK
  c![keyPath: kp] = str // OK
  c?[keyPath: kp] = str // OK
}

class KeyPathBase {}
class KeyPathBaseSubtype: KeyPathBase {}
class AnotherBase {}
class AnotherComposeBase {
  var member: KeyPathBase?
}

func key_path_root_mismatch<T>(_ base: KeyPathBase?, subBase: KeyPathBaseSubtype?, _ abase: AnotherComposeBase,
                               _ kp: KeyPath<KeyPathBase, T>, _ kpa: KeyPath<AnotherBase, T>) {
  let _ : T = base[keyPath: kp] // expected-error {{value of optional type 'KeyPathBase?' must be unwrapped to a value of type 'KeyPathBase'}}
  // expected-note@-1 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{19-19=!}}
  // expected-note@-2 {{use '?' to access key path subscript only for non-'nil' base values}} {{19-19=?}}
  let _ : T = base[keyPath: kpa] // expected-error {{key path with root type 'AnotherBase' cannot be applied to a base of type 'KeyPathBase?'}}

  // Chained root mismatch
  let _ : T = abase.member[keyPath: kp] // expected-error {{value of optional type 'KeyPathBase?' must be unwrapped to a value of type 'KeyPathBase'}}
  // expected-note@-1 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{27-27=!}}
  // expected-note@-2 {{use '?' to access key path subscript only for non-'nil' base values}} {{27-27=?}}
  let _ : T = abase.member[keyPath: kpa] // expected-error {{key path with root type 'AnotherBase' cannot be applied to a base of type 'KeyPathBase?'}}

  let _ : T = subBase[keyPath: kp] // expected-error {{value of optional type 'KeyPathBaseSubtype?' must be unwrapped to a value of type 'KeyPathBaseSubtype'}}
  // expected-note@-1 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{22-22=!}}
  // expected-note@-2 {{use '?' to access key path subscript only for non-'nil' base values}} {{22-22=?}}
  let _ : T = subBase[keyPath: kpa] // expected-error {{key path with root type 'AnotherBase' cannot be applied to a base of type 'KeyPathBaseSubtype?'}}

}

// SR-13442
func SR13442<T>(_ x: KeyPath<String?, T>) -> T { "1"[keyPath: x] }

func testSR13442() {
  _ = SR13442(\.!.count) // OK
  _ = SR13442(\String?.!.count) // OK
}
