// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -swift-version 4 -typecheck %t/declarations.swift %t/other_file_extensions.swift -verify

// BEGIN declarations.swift
struct PrivateMembers  {
  private var privateCounter: Int = 0 // expected-note 2 {{declared here}}
  private func privateMethod() {} // expected-note 2 {{declared here}}
  private struct PrivateInner { // expected-note 3 {{declared here}}
    private struct Invisible {} // expected-note {{declared here}}
  }
}
extension PrivateMembers {
  private func usePrivate() { // expected-note 2 {{declared here}}
    print(privateCounter)
    privateMethod()
    _ = PrivateInner()
    _ = PrivateInner.Invisible() // expected-error {{'Invisible' is inaccessible due to 'private' protection level}}
  }
}
func using(_ obj: PrivateMembers) {
  print(obj.privateCounter) // expected-error {{'privateCounter' is inaccessible due to 'private' protection level}}
  obj.privateMethod() // expected-error {{'privateMethod' is inaccessible due to 'private' protection level}}
  obj.usePrivate() // expected-error {{'usePrivate' is inaccessible due to 'private' protection level}}
  _ = PrivateMembers.PrivateInner() // expected-error {{'PrivateInner' is inaccessible due to 'private' protection level}}
  _ = PrivateMembers.PrivateInner.Invisible() // expected-error {{'PrivateInner' is inaccessible due to 'private' protection level}}
}

struct Outer {
  private static func privateDeclaration() {}
  struct Middle {
    private static func privateDeclaration() {}
    struct Inner {
      private static func privateDeclaration() {}
    }
  }
}

extension Outer.Middle.Inner {
  func useParentDeclarationPrivate() {
    Outer.privateDeclaration()
    Outer.Middle.privateDeclaration()
    Outer.Middle.Inner.privateDeclaration()
  }
}

// BEGIN other_file_extensions.swift
extension PrivateMembers {
  private func useDeclarationPrivate() {
    print(privateCounter) // expected-error {{'privateCounter' is inaccessible due to 'private' protection level}}
    privateMethod() // expected-error {{'privateMethod' is inaccessible due to 'private' protection level}}
    usePrivate() // expected-error {{'usePrivate' is inaccessible due to 'private' protection level}}
    _ = PrivateInner() // expected-error {{'PrivateInner' is inaccessible due to 'private' protection level}}
  }
}

extension PrivateMembers {
  private func useExtensionPrivate() {
    useDeclarationPrivate()
  }
}

extension Outer {
  private struct MiddleExtension {
    private static func privateDeclaration() {} // expected-note {{declared here}}
  }
  private static func outerExtension() {}
}

extension Outer.Middle.Inner {
  func useParentExtensionPrivate() {
    Outer.outerExtension()
    _ = Outer.MiddleExtension()
    Outer.MiddleExtension.privateDeclaration() // expected-error {{'privateDeclaration' is inaccessible due to 'private' protection level}}
  }
}
