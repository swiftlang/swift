// RUN: %target-typecheck-verify-swift

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

private // expected-note 3 {{modifier already specified here}}
public // expected-error {{duplicate modifier}}
internal // expected-error {{duplicate modifier}}
fileprivate // expected-error {{duplicate modifier}}
func quadruplicateAttrChanged() {}

private(set)
public
var customSetter = 0

fileprivate(set)
public
var customSetter2 = 0

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
duplicateAttr(1) // expected-error{{argument passed to call that takes no arguments}}

// CHECK ALLOWED DECLS
private import Swift // expected-error {{'private' modifier cannot be applied to this declaration}} {{1-9=}}
private(set) infix operator ~~~ // expected-error {{'private' modifier cannot be applied to this declaration}} {{1-14=}}

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
  internal deinit {} // expected-error {{'internal' modifier cannot be applied to this declaration}} {{3-12=}}
}

private enum TestEnum {
  private case Foo, Bar // expected-error {{'private' modifier cannot be applied to this declaration}} {{3-11=}}
}

private protocol TestProtocol {
  private associatedtype Foo // expected-error {{'private' modifier cannot be applied to this declaration}} {{3-11=}}
  internal var Bar: Int { get } // expected-error {{'internal' modifier cannot be used in protocols}} {{3-12=}}
  // expected-note@-1 {{protocol requirements implicitly have the same access as the protocol itself}}
  public func baz() // expected-error {{'public' modifier cannot be used in protocols}} {{3-10=}}
  // expected-note@-1 {{protocol requirements implicitly have the same access as the protocol itself}}
}

public(set) func publicSetFunc() {} // expected-error {{'public' modifier cannot be applied to this declaration}} {{1-13=}}

public(set) var defaultVis = 0 // expected-error {{internal variable cannot have a public setter}}
internal(set) private var privateVis = 0 // expected-error {{private variable cannot have an internal setter}}
private(set) var defaultVisOK = 0
private(set) public var publicVis = 0

private(set) var computed: Int { // expected-error {{'private(set)' modifier cannot be applied to read-only variables}} {{1-14=}}
  return 42
}
private(set) var computedRW: Int {
  get { return 42 }
  set { }
}
private(set) let constant = 42 // expected-error {{'private(set)' modifier cannot be applied to constants}} {{1-14=}}

public struct Properties {
  private(set) var stored = 42
  private(set) var computed: Int { // expected-error {{'private(set)' modifier cannot be applied to read-only properties}} {{3-16=}}
    return 42
  }
  private(set) var computedRW: Int {
    get { return 42 }
    set { }
  }
  private(set) let constant = 42 // expected-error {{'private(set)' modifier cannot be applied to read-only properties}} {{3-16=}}
  public(set) var defaultVis = 0 // expected-error {{internal property cannot have a public setter}}
  open(set) var defaultVis2 = 0 // expected-error {{internal property cannot have an open setter}}

  public(set) subscript(a a: Int) -> Int { // expected-error {{internal subscript cannot have a public setter}}
    get { return 0 }
    set {}
  }
  internal(set) private subscript(b b: Int) -> Int { // expected-error {{private subscript cannot have an internal setter}}
    get { return 0 }
    set {}
  }
  private(set) subscript(c c: Int) -> Int {
    get { return 0 }
    set {}
  }
  private(set) public subscript(d d: Int) -> Int {
    get { return 0 }
    set {}
  }

  private(set) subscript(e e: Int) -> Int { return 0 } // expected-error {{'private(set)' modifier cannot be applied to read-only subscripts}} {{3-16=}}
}

private extension Properties {
  public(set) var extProp: Int { // expected-error {{private property cannot have a public setter}}
    get { return 42 }
    set { }
  }
  open(set) var extProp2: Int { // expected-error {{private property cannot have an open setter}}
    get { return 42 }
    set { }
  }
}

internal protocol EmptyProto {}
internal protocol EmptyProto2 {}
private extension Properties : EmptyProto {} // expected-error {{'private' modifier cannot be used with extensions that declare protocol conformances}} {{1-9=}}
private(set) extension Properties : EmptyProto2 {} // expected-error {{'private' modifier cannot be applied to this declaration}} {{1-14=}}

public struct PublicStruct {}
internal struct InternalStruct {} // expected-note * {{declared here}}
private struct PrivateStruct {} // expected-note * {{declared here}}

protocol InternalProto { // expected-note * {{declared here}}
  associatedtype Assoc
}
public extension InternalProto {} // expected-error {{extension of internal protocol cannot be declared public}} {{1-8=}}
internal extension InternalProto where Assoc == PublicStruct {}
internal extension InternalProto where Assoc == InternalStruct {}
internal extension InternalProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared internal because its generic requirement uses a private type}}
private extension InternalProto where Assoc == PublicStruct {}
private extension InternalProto where Assoc == InternalStruct {}
private extension InternalProto where Assoc == PrivateStruct {}

public protocol PublicProto {
  associatedtype Assoc
}
public extension PublicProto {}
public extension PublicProto where Assoc == PublicStruct {}
public extension PublicProto where Assoc == InternalStruct {} // expected-error {{extension cannot be declared public because its generic requirement uses an internal type}}
public extension PublicProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared public because its generic requirement uses a private type}}
internal extension PublicProto where Assoc == PublicStruct {}
internal extension PublicProto where Assoc == InternalStruct {}
internal extension PublicProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared internal because its generic requirement uses a private type}}
private extension PublicProto where Assoc == PublicStruct {}
private extension PublicProto where Assoc == InternalStruct {}
private extension PublicProto where Assoc == PrivateStruct {}

extension PublicProto where Assoc == InternalStruct {
  public func foo() {} // expected-error {{cannot declare a public instance method in an extension with internal requirements}} {{3-9=internal}}
  open func bar() {} // expected-error {{cannot declare an open instance method in an extension with internal requirements}} {{3-7=internal}}
}
extension InternalProto {
  public func foo() {} // no effect, but no warning
}
extension InternalProto where Assoc == PublicStruct {
  public func foo() {} // expected-error {{cannot declare a public instance method in an extension with internal requirements}} {{3-9=internal}}
  open func bar() {} // expected-error {{cannot declare an open instance method in an extension with internal requirements}} {{3-7=internal}}
}

public struct GenericStruct<Param> {}
public extension GenericStruct where Param: InternalProto {} // expected-error {{extension cannot be declared public because its generic requirement uses an internal type}}
extension GenericStruct where Param: InternalProto {
  public func foo() {} // expected-error {{cannot declare a public instance method in an extension with internal requirements}} {{3-9=internal}}
}


public protocol ProtoWithReqs {
  associatedtype Assoc
  func foo()
}

public struct Adopter<T> : ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'Assoc' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
extension Adopter {
  typealias Assoc = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-3=public }}
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}

public class AnotherAdopterBase {
  typealias Assoc = Int
  // expected-note@-1 {{mark the type alias as 'public' to satisfy the requirement}} {{3-3=public }}
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}
public class AnotherAdopterSub : AnotherAdopterBase, ProtoWithReqs {}
// expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
// expected-error@-2 {{type alias 'Assoc' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}

public protocol ReqProvider {}
extension ReqProvider {
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}
public struct AdoptViaProtocol : ProtoWithReqs, ReqProvider {
  // expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
  public typealias Assoc = Int
}

public protocol ReqProvider2 {}
extension ProtoWithReqs where Self : ReqProvider2 {
  func foo() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}
public struct AdoptViaCombinedProtocol : ProtoWithReqs, ReqProvider2 {
  // expected-error@-1 {{method 'foo()' must be declared public because it matches a requirement in public protocol 'ProtoWithReqs'}} {{none}}
  public typealias Assoc = Int
}

public protocol PublicInitProto {
  var value: Int { get }
  init(value: Int)
}
public struct NonPublicInitStruct: PublicInitProto {
  public var value: Int
  init(value: Int) {
  // expected-error@-1 {{initializer 'init(value:)' must be declared public because it matches a requirement in public protocol 'PublicInitProto'}}
  // expected-note@-2 {{mark the initializer as 'public' to satisfy the requirement}}
    self.value = value
  }
}
public struct NonPublicMemberwiseInitStruct: PublicInitProto {
// expected-error@-1 {{initializer 'init(value:)' must be declared public because it matches a requirement in public protocol 'PublicInitProto'}}
  public var value: Int
}
