// RUN: %target-typecheck-verify-swift

struct NonGenericStruct {
  typealias Horse = Int // expected-note {{'Horse' previously declared here}}
  typealias Horse = String // expected-error {{invalid redeclaration of 'Horse'}}
}

struct NonGenericExtendedStruct {}

extension NonGenericExtendedStruct {
  typealias Horse = Int // expected-note {{'Horse' previously declared here}}
}

extension NonGenericExtendedStruct {
  typealias Horse = String // expected-error {{invalid redeclaration of 'Horse'}}
}

struct GenericStruct<T> {
  typealias Horse = Int // expected-note {{'Horse' previously declared here}}
  typealias Horse = String // expected-error {{invalid redeclaration of 'Horse'}}
}

struct GenericExtendedStruct<T> {}

extension GenericExtendedStruct {
  typealias Horse = Int // expected-note {{'Horse' previously declared here}}
}

extension GenericExtendedStruct {
  typealias Horse = String // expected-error {{invalid redeclaration of 'Horse'}}
}

struct GenericConstrainedExtendedStruct<T> {}

protocol SomeProtocol {}

extension GenericConstrainedExtendedStruct where T : SomeProtocol {
  typealias Horse = Int
}

protocol OtherProtocol {}

extension GenericConstrainedExtendedStruct where T : OtherProtocol {
  typealias Horse = String // This is OK!
}
