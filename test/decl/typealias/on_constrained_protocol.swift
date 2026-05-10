// RUN: %target-typecheck-verify-swift

// typealias on constrained extension

protocol ConstrainedTypealias {
  associatedtype MyAssocType	
}
extension ConstrainedTypealias where MyAssocType == String { // expected-note {{requirement specified as 'Self.MyAssocType' == 'String' [with Self = Self]}} (from useConstrainedTypealiasInExtension)
  typealias Content = String 
}
extension ConstrainedTypealias where MyAssocType == Int {
  func useConstrainedTypealiasInExtension() -> Content {} // expected-error {{'Self.Content' (aka 'String') requires the types 'Int' and 'String' be equivalent}}
}
func useTypealiasOnConstrainedExtension() -> ConstrainedTypealias.Content {}

// define different typealiases on differently constrained extensions

protocol DoubleOverloadedTypealias {
  associatedtype MyAssocType
}
extension DoubleOverloadedTypealias where MyAssocType == String { // expected-note {{requirement specified as 'Self.MyAssocType' == 'String' [with Self = Self]}} (from useDoubleOverloadedTypealiasInExtension)
  typealias Content = String // expected-note {{found candidate with type 'String'}} (from useDoubleOverloadedTypealias)
}
extension DoubleOverloadedTypealias where MyAssocType == Int {
  typealias Content = Int // expected-note {{found candidate with type 'Int'}} (from useDoubleOverloadedTypealias)
  func useDoubleOverloadedTypealiasInExtension() -> Content {} // expected-error {{'Self.Content' (aka 'String') requires the types 'Int' and 'String' be equivalent}}
}
func useDoubleOverloadedTypealias() -> DoubleOverloadedTypealias.Content {} // expected-error {{ambiguous type name 'Content' in 'DoubleOverloadedTypealias'}}

// define the same typealias on differently constrained extensions

protocol DoubleOverloadedSameTypealias {
  associatedtype MyAssocType
}
extension DoubleOverloadedSameTypealias where MyAssocType == String { // expected-note {{requirement specified as 'Self.MyAssocType' == 'String' [with Self = Self]}} (from useDoubleOverloadedSameTypealiasInExtension)
  typealias Content = Int
}
extension DoubleOverloadedSameTypealias where MyAssocType == Int {
  typealias Content = Int
  func useDoubleOverloadedSameTypealiasInExtension() -> Content {} // expected-error {{'Self.Content' (aka 'Int') requires the types 'Int' and 'String' be equivalent}}
}
func useDoubleOverloadedSameTypealias() -> DoubleOverloadedSameTypealias.Content {}

// Overload associatedtype with typealias
// https://github.com/apple/swift/issues/50805

protocol MarkerProtocol {}

protocol ProtocolWithAssoctype {
  associatedtype MyAssocType
}

extension ProtocolWithAssoctype where Self: MarkerProtocol {
  typealias MyAssocType = Int

  func useAssocTypeInExtension(_ a: Self.MyAssocType) -> MyAssocType {
    return a
  }

  func useAssocMetatypeInExtensionUnqualified() -> MyAssocType.Type {
    return MyAssocType.self
  }

  func useAssocMetatypeInExtensionQualified() -> Self.MyAssocType.Type {
    return Self.MyAssocType.self
  }
}

func useAssocTypeOutsideExtension() -> ProtocolWithAssoctype.MyAssocType {}
// expected-error@-1 {{cannot access associated type 'MyAssocType' from 'ProtocolWithAssoctype'; use a concrete type or generic parameter base instead}}
