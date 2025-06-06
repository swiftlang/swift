// RUN: %target-typecheck-verify-swift -package-name myPkg

// CHECK PARSING
private // expected-note {{modifier already specified here}}
private // expected-error {{duplicate modifier}}
func duplicateAttr() {}

private // expected-note {{previous modifier specified here}}
public // expected-error {{multiple incompatible access-level modifiers specified}}
func duplicateAttrChanged() {}

private // expected-note 2 {{previous modifier specified here}}
public // expected-error {{multiple incompatible access-level modifiers specified}}
internal // expected-error {{multiple incompatible access-level modifiers specified}}
func triplicateAttrChanged() {}

private // expected-note 3 {{previous modifier specified here}}
public // expected-error {{multiple incompatible access-level modifiers specified}}
package // expected-error {{multiple incompatible access-level modifiers specified}}
internal // expected-error {{multiple incompatible access-level modifiers specified}}
func quadruplicateAttrChanged() {}

private // expected-note 4 {{previous modifier specified here}}
public // expected-error {{multiple incompatible access-level modifiers specified}}
package // expected-error {{multiple incompatible access-level modifiers specified}}
internal // expected-error {{multiple incompatible access-level modifiers specified}}
fileprivate // expected-error {{multiple incompatible access-level modifiers specified}}
func quintuplicateAttrChanged() {}

private(set)
public
var customSetter = 0

fileprivate(set)
public
var customSetter2 = 0

package(set)
public
var customSetter3 = 0

public
package(set)
var customSetter4 = 0

private(set)
package
var customSetter5 = 0

internal(set)
package
var customSetter6 = 0

private(set) // expected-note {{previous modifier specified here}}
public(set) // expected-error {{multiple incompatible access-level modifiers specified}}
var customSetterDuplicateAttr = 0

private(set) // expected-note {{previous modifier specified here}}
public // expected-note {{previous modifier specified here}}
public(set) // expected-error {{multiple incompatible access-level modifiers specified}}
private // expected-error {{multiple incompatible access-level modifiers specified}}
var customSetterDuplicateAttrsAllAround = 0

private(set) // expected-note {{previous modifier specified here}}
package(set) // expected-error {{multiple incompatible access-level modifiers specified}}
var customSetterDuplicateAttr2 = 0

package(set) // expected-note {{previous modifier specified here}}
public(set) // expected-error {{multiple incompatible access-level modifiers specified}}
public var customSetterDuplicateAttr3 = 0

private(get) // expected-error{{expected 'set' as subject of 'private' modifier}} {{9-12=set}}
var invalidSubject = 0

private(42) // expected-error{{expected 'set' as subject of 'private' modifier}} {{9-11=set}}
var invalidSubject2 = 0

private(a bunch of random tokens) // expected-error{{expected 'set' as subject of 'private' modifier}} expected-error{{expected declaration}}
var invalidSubject3 = 0


package(get) // expected-error{{expected 'set' as subject of 'package' modifier}} {{9-12=set}}
var invalidSubject4 = 0

package(42) // expected-error{{expected 'set' as subject of 'package' modifier}} {{9-11=set}}
var invalidSubject5 = 0

private((())) // expected-error{{expected 'set' as subject of 'private' modifier}} expected-error{{expected declaration}}
var invalidSubject6 = 0

private( missingFunc(_ x: Int) -> Bool // expected-error{{expected 'set' as subject of 'private' modifier}} {{9-9=set)}} expected-error{{expected declaration}}
let independentVar1 = 0

private(set // expected-error{{expected ')' in 'private' modifier}} {{12-12=)}}
var unterminatedSubject = 0

private(42 // expected-error{{expected 'set' as subject of 'private' modifier}} {{9-11=set)}} expected-error{{expected declaration}}
var unterminatedInvalidSubject = 0

private() // expected-error{{expected 'set' as subject of 'private' modifier}} {{9-9=set}}
var emptySubject = 0

private( // expected-error{{expected 'set' as subject of 'private' modifier}} {{9-9=set)}}
var unterminatedEmptySubject = 0

// Check that the parser made it here.
duplicateAttr(1) // expected-error{{argument passed to call that takes no arguments}}

// CHECK ALLOWED DECLS
private import Swift
private(set) infix operator ~~~ // expected-error {{unexpected attribute 'private' in operator declaration}} {{1-14=}}

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

package enum PkgTestEnum {
  package case Foo, Bar // expected-error {{'package' modifier cannot be applied to this declaration}} {{3-11=}}
}

private protocol TestProtocol {
  private associatedtype Foo // expected-error {{'private' modifier cannot be applied to this declaration}} {{3-11=}}
  internal var Bar: Int { get } // expected-error {{'internal' modifier cannot be used in protocols}} {{3-12=}}
  // expected-note@-1 {{protocol requirements implicitly have the same access as the protocol itself}}
  package func cat() // expected-error {{'package' modifier cannot be used in protocols}} {{3-11=}}
  // expected-note@-1 {{protocol requirements implicitly have the same access as the protocol itself}}
  public func baz() // expected-error {{'public' modifier cannot be used in protocols}} {{3-10=}}
  // expected-note@-1 {{protocol requirements implicitly have the same access as the protocol itself}}
}

public(set) func publicSetFunc() {} // expected-error {{'public' modifier cannot be applied to this declaration}} {{1-13=}}

public(set) var defaultVis = 0 // expected-error {{internal variable cannot have a public setter}}
package(set) var defaultVisPkg = 0 // expected-error {{internal variable cannot have a package setter}}
package(set) package var defaultVisPkgPkg = 0 // expected-warning {{'package(set)' modifier is redundant for a package var}}
package(set) public var defaultVisPkgOK = 0 // OK
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

package(set) public var computedPkg: Int { // expected-error {{'package(set)' modifier cannot be applied to read-only variables}} {{1-14=}}
  return 42
}

private(set) let constant = 42 // expected-error {{'private(set)' modifier cannot be applied to constants}} {{1-14=}}

package(set) public let constantPkg = 42 // expected-error {{'package(set)' modifier cannot be applied to constants}} {{1-14=}}

public struct Properties {
  private(set) var stored = 42
  private(set) var computed: Int { // expected-error {{'private(set)' modifier cannot be applied to read-only properties}} {{3-16=}}
    return 42
  }
  private(set) var computedRW: Int {
    get { return 42 }
    set { }
  }

  package(set) public var computedRWPkg: Int {
    get { return 42 }
    set { }
  }

  public package(set) var computedR: Int { // expected-error {{'package(set)' modifier cannot be applied to read-only properties}} {{10-23=}}
    return 42
  }

  private(set) let constant = 42 // expected-error {{'private(set)' modifier cannot be applied to read-only properties}} {{3-16=}}
  package(set) public let constantPkg = 42 // expected-error {{'package(set)' modifier cannot be applied to read-only properties}} {{3-16=}}
  package(set) var defaultVisPkg = 42 // expected-error {{internal property cannot have a package setter}}
  public(set) var defaultVis = 0 // expected-error {{internal property cannot have a public setter}}
  open(set) var defaultVis2 = 0 // expected-error {{internal property cannot have an open setter}}

  public(set) subscript(a a: Int) -> Int { // expected-error {{internal subscript cannot have a public setter}}
    get { return 0 }
    set {}
  }
  package(set) subscript(p p: Int) -> Int { // expected-error {{internal subscript cannot have a package setter}}
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
  package(set) var extPropPkg: Int { // expected-error {{private property cannot have a package setter}}
    get { return 42 }
    set { }
  }
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

package protocol EmptyProto3 {}
package protocol EmptyProto4 {}
public protocol EmptyProto5 {}
public protocol EmptyProto6 {}

private extension Properties : EmptyProto3 {} // expected-error {{'private' modifier cannot be used with extensions that declare protocol conformances}} {{1-9=}}
private(set) extension Properties : EmptyProto4 {} // expected-error {{'private' modifier cannot be applied to this declaration}} {{1-14=}}

package extension Properties : EmptyProto5 {} // expected-error {{'package' modifier cannot be used with extensions that declare protocol conformances}} {{1-9=}}
package(set) extension Properties : EmptyProto6 {} // expected-error {{'package' modifier cannot be applied to this declaration}} {{1-14=}}

public struct PublicStruct {}
package struct PackageStruct {} // expected-note * {{declared here}}
internal struct InternalStruct {} // expected-note * {{declared here}}
private struct PrivateStruct {} // expected-note * {{declared here}}

protocol InternalProto { // expected-note * {{declared here}}
  associatedtype Assoc
}
package protocol PackageProto { // expected-note * {{declared here}}
  associatedtype Assoc
}
public extension InternalProto {} // expected-error {{extension of internal protocol cannot be declared public}} {{1-8=}}
public extension PackageProto {} // expected-error {{extension of package protocol cannot be declared public}} {{1-8=}}
package extension PackageProto {} // no-error
package extension InternalProto {} // expected-error {{extension of internal protocol cannot be declared package}} {{1-9=}}
package extension PackageProto where Assoc == PublicStruct {}
package extension PackageProto where Assoc == PackageStruct {}
package extension PackageProto where Assoc == InternalStruct {} // expected-error {{extension cannot be declared package because its generic requirement uses an internal type}}
package extension PackageProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared package because its generic requirement uses a private type}}
internal extension PackageProto where Assoc == PublicStruct {}
internal extension PackageProto where Assoc == PackageStruct {}
internal extension PackageProto where Assoc == InternalStruct {}
internal extension PackageProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared internal because its generic requirement uses a private type}}
private extension PackageProto where Assoc == PublicStruct {}
private extension PackageProto where Assoc == PackageStruct {}
private extension PackageProto where Assoc == InternalStruct {}
private extension PackageProto where Assoc == PrivateStruct {}

internal extension InternalProto where Assoc == PublicStruct {}
internal extension InternalProto where Assoc == PackageStruct {}
internal extension InternalProto where Assoc == InternalStruct {}
internal extension InternalProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared internal because its generic requirement uses a private type}}
private extension InternalProto where Assoc == PublicStruct {}
private extension InternalProto where Assoc == PackageStruct {}
private extension InternalProto where Assoc == InternalStruct {}
private extension InternalProto where Assoc == PrivateStruct {}

public protocol PublicProto {
  associatedtype Assoc
}
public extension PublicProto {}
public extension PublicProto where Assoc == PublicStruct {}
public extension PublicProto where Assoc == PackageStruct {} // expected-error {{extension cannot be declared public because its generic requirement uses a package type}}
public extension PublicProto where Assoc == InternalStruct {} // expected-error {{extension cannot be declared public because its generic requirement uses an internal type}}
public extension PublicProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared public because its generic requirement uses a private type}}

package extension PublicProto where Assoc == PublicStruct {}
package extension PublicProto where Assoc == PackageStruct {}
package extension PublicProto where Assoc == InternalStruct {} // expected-error {{extension cannot be declared package because its generic requirement uses an internal type}}
package extension PublicProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared package because its generic requirement uses a private type}}

internal extension PublicProto where Assoc == PublicStruct {}
internal extension PublicProto where Assoc == PackageStruct {}
internal extension PublicProto where Assoc == InternalStruct {}
internal extension PublicProto where Assoc == PrivateStruct {} // expected-error {{extension cannot be declared internal because its generic requirement uses a private type}}
private extension PublicProto where Assoc == PublicStruct {}
private extension PublicProto where Assoc == PackageStruct {}
private extension PublicProto where Assoc == InternalStruct {}
private extension PublicProto where Assoc == PrivateStruct {}

extension PublicProto where Assoc == PackageStruct {
  public func foo() {} // expected-error {{cannot declare a public instance method in an extension with package requirements}} {{3-9=package}}
  open func bar() {} // expected-error {{cannot declare an open instance method in an extension with package requirements}} {{3-7=package}}
}

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
extension InternalProto where Assoc == PackageStruct {
  public func foo() {} // expected-error {{cannot declare a public instance method in an extension with internal requirements}} {{3-9=internal}}
  open func bar() {} // expected-error {{cannot declare an open instance method in an extension with internal requirements}} {{3-7=internal}}
}

public struct GenericStruct<Param> {}
public extension GenericStruct where Param: InternalProto {} // expected-error {{extension cannot be declared public because its generic requirement uses an internal type}}
extension GenericStruct where Param: InternalProto {
  public func foo() {} // expected-error {{cannot declare a public instance method in an extension with internal requirements}} {{3-9=internal}}
}
public extension GenericStruct where Param: PackageProto {} // expected-error {{extension cannot be declared public because its generic requirement uses a package type}}
extension GenericStruct where Param: PackageProto {
  public func foo() {} // expected-error {{cannot declare a public instance method in an extension with package requirements}} {{3-9=package}}
}

package struct PkgGenericStruct<Param> {}
package extension PkgGenericStruct where Param: InternalProto {} // expected-error {{extension cannot be declared package because its generic requirement uses an internal type}}
extension PkgGenericStruct where Param: InternalProto {
  package func foo() {} // expected-error {{cannot declare a package instance method in an extension with internal requirements}} {{3-10=internal}}
}
