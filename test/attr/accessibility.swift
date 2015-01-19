// RUN: %target-parse-verify-swift

// CHECK PARSING
private // expected-note {{modifier already specified here}}
private // expected-error {{duplicate modifier}}
func duplicateAttr() {}

private // expected-note {{modifier already specified here}}
public // expected-error {{duplicate modifier}}
func duplicateAttrChanged() {}

private // expected-note 2 {{modifier already specified here}}
public // expected-error {{duplicate modifier}}
internal // expected-error {{duplicate modifier}}
func triplicateAttrChanged() {}

private(set)
public
var customSetter = 0

private(set) // expected-note {{modifier already specified here}}
public(set) // expected-error {{duplicate modifier}}
var customSetterDuplicateAttr = 0

private(set) // expected-note {{modifier already specified here}}
public // expected-note {{modifier already specified here}}
public(set) // expected-error {{duplicate modifier}}
private // expected-error {{duplicate modifier}}
var customSetterDuplicateAttrsAllAround = 0

private(get) // expected-error{{expected 'set' as subject of 'private' modifier}}
var invalidSubject = 0

private(42) // expected-error{{expected 'set' as subject of 'private' modifier}}
var invalidSubject2 = 0

private(a bunch of random tokens) // expected-error{{expected 'set' as subject of 'private' modifier}} expected-error{{expected declaration}}
var invalidSubject3 = 0

private(set // expected-error{{expected ')' in 'private' modifier}}
var unterminatedSubject = 0

private(42 // expected-error{{expected 'set' as subject of 'private' modifier}} expected-error{{expected declaration}}
var unterminatedInvalidSubject = 0

private() // expected-error{{expected 'set' as subject of 'private' modifier}}
var emptySubject = 0

private( // expected-error{{expected 'set' as subject of 'private' modifier}}
var unterminatedEmptySubject = 0

// Check that the parser made it here.
duplicateAttr(1) // expected-error{{}}


// CHECK ALLOWED DECLS
private import Swift // expected-error {{'private' modifier cannot be applied to this declaration}}
private(set) infix operator ~~~ {} // expected-error {{'private' modifier cannot be applied to this declaration}}

private typealias MyInt = Int

private struct TestStruct {
  private typealias LocalInt = MyInt
  private var x = 0
  private let y = 1
  private func method() {}
  private static func method() {}
  private init() {}
  private subscript(_: MyInt) -> LocalInt { return x }
}

private class TestClass {
  private init() {}
  internal deinit {} // expected-error {{'internal' modifier cannot be applied to this declaration}}
}

private enum TestEnum {
  private case Foo, Bar // expected-error {{'private' modifier cannot be applied to this declaration}}
}

private protocol TestProtocol {
  private typealias Foo // expected-error {{'private' modifier cannot be applied to this declaration}}
  internal var Bar: Int { get } // expected-error {{'internal' modifier cannot be used in protocols}}
  public func baz() // expected-error {{'public' modifier cannot be used in protocols}}
}

public(set) func publicSetFunc() {} // expected-error {{'public' modifier cannot be applied to this declaration}}

public(set) var defaultVis = 0 // expected-error {{internal variable cannot have a public setter}}
internal(set) private var privateVis = 0 // expected-error {{private variable cannot have an internal setter}}
private(set) var defaultVisOK = 0
private(set) public var publicVis = 0

private(set) var computed: Int { // expected-error {{'private(set)' modifier cannot be applied to read-only variables}}
  return 42
}
private(set) var computedRW: Int {
  get { return 42 }
  set { }
}
private(set) let constant = 42 // expected-error {{'private(set)' modifier cannot be applied to constants}}

public struct Properties {
  private(set) var stored = 42
  private(set) var computed: Int { // expected-error {{'private(set)' modifier cannot be applied to read-only properties}}
    return 42
  }
  private(set) var computedRW: Int {
    get { return 42 }
    set { }
  }
  private(set) let constant = 42 // expected-error {{'private(set)' modifier cannot be applied to read-only properties}}
  public(set) var defaultVis = 0 // expected-error {{internal property cannot have a public setter}}

  public(set) subscript(#a: Int) -> Int { // expected-error {{internal subscript cannot have a public setter}}
    get { return 0 }
    set {}
  }
  internal(set) private subscript(#b: Int) -> Int { // expected-error {{private subscript cannot have an internal setter}}
    get { return 0 }
    set {}
  }
  private(set) subscript(#c: Int) -> Int {
    get { return 0 }
    set {}
  }
  private(set) public subscript(#d: Int) -> Int {
    get { return 0 }
    set {}
  }

  private(set) subscript(#e: Int) -> Int { return 0 } // expected-error {{'private(set)' modifier cannot be applied to read-only subscripts}}
}

private extension Properties {
  public(set) var extProp: Int { // expected-error {{private property cannot have a public setter}}
    get { return 42 }
    set { }
  }
}

internal protocol EmptyProto {}
private extension Properties : EmptyProto {} // expected-error {{'private' modifier cannot be used with extensions that declare protocol conformances}}
private(set) extension Properties : EmptyProto {} // expected-error {{'private' modifier cannot be applied to this declaration}}
