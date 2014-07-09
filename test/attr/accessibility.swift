// RUN: %swift %s -parse -verify

// CHECK PARSING
private // expected-note {{attribute already specified here}}
private // expected-error {{duplicate attribute}}
func duplicateAttr() {}

private // expected-note {{attribute already specified here}}
public // expected-error {{duplicate attribute}}
func duplicateAttrChanged() {}

private // expected-note 2 {{attribute already specified here}}
public // expected-error {{duplicate attribute}}
internal // expected-error {{duplicate attribute}}
func triplicateAttrChanged() {}

private(set)
public
var customSetter = 0

private(set) // expected-note {{attribute already specified here}}
public(set) // expected-error {{duplicate attribute}}
var customSetterDuplicateAttr = 0

private(set) // expected-note {{attribute already specified here}}
public // expected-note {{attribute already specified here}}
public(set) // expected-error {{duplicate attribute}}
private // expected-error {{duplicate attribute}}
var customSetterDuplicateAttrsAllAround = 0

// Check that the parser made it here.
duplicateAttr(1) // expected-error{{}}


// CHECK ALLOWED DECLS
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
  internal deinit {} // expected-error {{'internal' attribute cannot be applied to this declaration}}
}

private enum TestEnum {
  private case Foo, Bar // expected-error {{'private' attribute cannot be applied to this declaration}}
}

private protocol TestProtocol {
  private typealias Foo // expected-error {{'private' attribute cannot be used in protocols}}
  internal var Bar: Int { get } // expected-error {{'internal' attribute cannot be used in protocols}}
  public func baz() // expected-error {{'public' attribute cannot be used in protocols}}
}

public(set) func publicSetFunc() {} // expected-error {{'public(set)' attribute can only be applied to variables and subscripts}}

public(set) var defaultVis = 0 // expected-error {{internal variable cannot have a public setter}}
internal(set) private var privateVis = 0 // expected-error {{private variable cannot have an internal setter}}
private(set) var defaultVisOK = 0
private(set) public var publicVis = 0

private(set) var computed: Int { // expected-error {{'private(set)' attribute cannot be applied to read-only variables}}
  return 42
}
private(set) var computedRW: Int {
  get { return 42 }
  set { }
}
private(set) let constant = 42 // expected-error {{'private(set)' attribute cannot be applied to constants}}

public struct Properties {
  private(set) var stored = 42
  private(set) var computed: Int { // expected-error {{'private(set)' attribute cannot be applied to read-only properties}}
    return 42
  }
  private(set) var computedRW: Int {
    get { return 42 }
    set { }
  }
  private(set) let constant = 42 // expected-error {{'private(set)' attribute cannot be applied to read-only properties}}
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

  private(set) subscript(#e: Int) -> Int { return 0 } // expected-error {{'private(set)' attribute cannot be applied to read-only subscripts}}
}

private extension Properties {
  public(set) var extProp: Int { // expected-error {{private property cannot have a public setter}}
    get { return 42 }
    set { }
  }
}

internal protocol EmptyProto {}
private extension Properties : EmptyProto {} // expected-error {{'private' attribute cannot be used with extensions that declare protocol conformances}}
private(set) extension Properties : EmptyProto {} // expected-error {{'private(set)' attribute can only be applied to variables and subscripts}}
