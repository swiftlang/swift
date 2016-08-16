// RUN: %target-parse-verify-swift

class Container {
  private func foo() {} // expected-note * {{declared here}}
  private var bar = 0 // expected-note * {{declared here}}

  private struct PrivateInner {} // expected-note * {{declared here}}

  func localTest() {
    foo()
    self.foo()

    _ = bar
    bar = 5
    _ = self.bar
    self.bar = 5

    privateExtensionMethod() // FIXME expected-error {{use of unresolved identifier 'privateExtensionMethod'}}
    self.privateExtensionMethod() // expected-error {{'privateExtensionMethod' is inaccessible due to 'private' protection level}}

    _ = PrivateInner()
    _ = Container.PrivateInner()
  }

  struct Inner {
    func test(obj: Container) {
      obj.foo()
      _ = obj.bar
      obj.bar = 5
      obj.privateExtensionMethod() // expected-error {{'privateExtensionMethod' is inaccessible due to 'private' protection level}}

      _ = PrivateInner()
      _ = Container.PrivateInner()
    }

    var inner: PrivateInner? // expected-error {{property must be declared private because its type uses a private type}}
    var innerQualified: Container.PrivateInner? // expected-error {{property must be declared private because its type uses a private type}}
  }

  var inner: PrivateInner? // expected-error {{property must be declared private because its type uses a private type}}
  var innerQualified: Container.PrivateInner? // expected-error {{property must be declared private because its type uses a private type}}
}

func test(obj: Container) {
  obj.foo() // expected-error {{'foo' is inaccessible due to 'private' protection level}}
  _ = obj.bar // expected-error {{'bar' is inaccessible due to 'private' protection level}}
  obj.bar = 5 // expected-error {{'bar' is inaccessible due to 'private' protection level}}
  obj.privateExtensionMethod() // expected-error {{'privateExtensionMethod' is inaccessible due to 'private' protection level}}

  _ = Container.PrivateInner() // expected-error {{'PrivateInner' is inaccessible due to 'private' protection level}}
}

extension Container {
  private func privateExtensionMethod() {} // expected-note * {{declared here}}

  func extensionTest() {
    foo() // FIXME expected-error {{use of unresolved identifier 'foo'}}
    self.foo() // expected-error {{'foo' is inaccessible due to 'private' protection level}}

    _ = bar // FIXME expected-error {{use of unresolved identifier 'bar'}}
    bar = 5 // FIXME expected-error {{use of unresolved identifier 'bar'}}
    _ = self.bar // expected-error {{'bar' is inaccessible due to 'private' protection level}}
    self.bar = 5 // expected-error {{'bar' is inaccessible due to 'private' protection level}}

    privateExtensionMethod()
    self.privateExtensionMethod()

    _ = PrivateInner() // FIXME expected-error {{use of unresolved identifier 'PrivateInner'}}
    _ = Container.PrivateInner() // expected-error {{'PrivateInner' is inaccessible due to 'private' protection level}}
  }

  // FIXME: Why do these errors happen twice?
  var extensionInner: PrivateInner? { return nil } // FIXME expected-error 2 {{use of undeclared type 'PrivateInner'}}
  var extensionInnerQualified: Container.PrivateInner? { return nil } // expected-error 2 {{'PrivateInner' is inaccessible due to 'private' protection level}}
}

extension Container.Inner {
  func extensionTest(obj: Container) {
    obj.foo() // expected-error {{'foo' is inaccessible due to 'private' protection level}}
    _ = obj.bar // expected-error {{'bar' is inaccessible due to 'private' protection level}}
    obj.bar = 5 // expected-error {{'bar' is inaccessible due to 'private' protection level}}
    obj.privateExtensionMethod() // expected-error {{'privateExtensionMethod' is inaccessible due to 'private' protection level}}

    _ = PrivateInner() // FIXME expected-error {{use of unresolved identifier 'PrivateInner'}}
    _ = Container.PrivateInner() // expected-error {{'PrivateInner' is inaccessible due to 'private' protection level}}
  }

  // FIXME: Why do these errors happen twice?
  var inner: PrivateInner? { return nil } // FIXME expected-error 2 {{use of undeclared type 'PrivateInner'}}
  var innerQualified: Container.PrivateInner? { return nil } // expected-error 2 {{'PrivateInner' is inaccessible due to 'private' protection level}}
}

class Sub : Container {
  func subTest() {
    foo() // FIXME expected-error {{use of unresolved identifier 'foo'}}
    self.foo() // expected-error {{'foo' is inaccessible due to 'private' protection level}}

    _ = bar // FIXME expected-error {{use of unresolved identifier 'bar'}}
    bar = 5 // FIXME expected-error {{use of unresolved identifier 'bar'}}
    _ = self.bar // expected-error {{'bar' is inaccessible due to 'private' protection level}}
    self.bar = 5 // expected-error {{'bar' is inaccessible due to 'private' protection level}}

    privateExtensionMethod() // FIXME expected-error {{use of unresolved identifier 'privateExtensionMethod'}}
    self.privateExtensionMethod() // expected-error {{'privateExtensionMethod' is inaccessible due to 'private' protection level}}

    _ = PrivateInner() // FIXME expected-error {{use of unresolved identifier 'PrivateInner'}}
    _ = Container.PrivateInner() // expected-error {{'PrivateInner' is inaccessible due to 'private' protection level}}
  }

  var subInner: PrivateInner? // FIXME expected-error {{use of undeclared type 'PrivateInner'}}
  var subInnerQualified: Container.PrivateInner? // expected-error {{'PrivateInner' is inaccessible due to 'private' protection level}}
}


protocol VeryImportantProto {
  associatedtype Assoc
  var value: Int { get set } // expected-note {{protocol requires property 'value' with type 'Int'; do you want to add a stub?}}
}

private struct VIPPrivateType : VeryImportantProto {
  private typealias Assoc = Int // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'VeryImportantProto'}}
  var value: Int
}

private struct VIPPrivateProp : VeryImportantProto {
  typealias Assoc = Int
  private var value: Int // expected-error {{property 'value' must be as accessible as its enclosing type because it matches a requirement in protocol 'VeryImportantProto'}} {{3-10=fileprivate}}
}

private struct VIPPrivateSetProp : VeryImportantProto {
  typealias Assoc = Int
  private(set) var value: Int // expected-error {{setter for property 'value' must be as accessible as its enclosing type because it matches a requirement in protocol 'VeryImportantProto'}} {{3-10=fileprivate}}
}

private class VIPPrivateSetBase {
  private var value: Int = 0
}
private class VIPPrivateSetSub : VIPPrivateSetBase, VeryImportantProto { // expected-error {{type 'VIPPrivateSetSub' does not conform to protocol 'VeryImportantProto'}}
  typealias Assoc = Int
}

private class VIPPrivateSetPropBase {
  private(set) var value: Int = 0 // expected-error {{setter for property 'value' must be as accessible as its enclosing type because it matches a requirement in protocol 'VeryImportantProto'}} {{3-10=fileprivate}}
}
private class VIPPrivateSetPropSub : VIPPrivateSetPropBase, VeryImportantProto {
  typealias Assoc = Int
}

extension Container {
  private typealias ExtensionConflictingType = Int // expected-note * {{declared here}}
}
extension Container {
  private typealias ExtensionConflictingType = Double // expected-note * {{declared here}}
}
extension Container {
  func test() {
    let a: ExtensionConflictingType? = nil // FIXME expected-error {{use of undeclared type 'ExtensionConflictingType'}}
    let b: Container.ExtensionConflictingType? = nil // expected-error {{'ExtensionConflictingType' is inaccessible due to 'private' protection level}}
    _ = ExtensionConflictingType() // FIXME expected-error {{use of unresolved identifier 'ExtensionConflictingType'}}
    _ = Container.ExtensionConflictingType() // expected-error {{'ExtensionConflictingType' is inaccessible due to 'private' protection level}}
  }
}
