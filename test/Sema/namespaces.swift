// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature Namespaces
// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces

namespace Values {
  static func answer() -> Int { 42 }
}

let unqualifiedAnswer = answer() // expected-error {{cannot find 'answer' in scope}}
let bareNamespace = Values // expected-error {{expected namespace member name after namespace name}}
typealias NamespaceAlias = Values // expected-error {{cannot use namespace 'Values' as a type}}
typealias NamespaceMetatype = Values.Type // expected-error {{cannot use namespace 'Values' as a type}}
typealias NamespaceGenericArgument = Array<Values> // expected-error {{cannot use namespace 'Values' as a type}}
let constructedNamespace = Values() // expected-error {{cannot call value of non-function type 'namespace<Values>'}}

namespace Redeclarations {
  static func duplicate() {} // expected-note {{'duplicate()' previously declared here}}
  static func duplicate() {} // expected-error {{invalid redeclaration of 'duplicate()'}}
}

namespace MissingStatic {
  func answer() -> Int { 42 } // expected-error {{function 'answer()' declared in a namespace must be marked 'static'}}
}

namespace ClassSpelling {
  class func answer() -> Int { 42 } // expected-error {{'class' methods are not allowed in a namespace; use 'static' instead}}
}

namespace UnsupportedStorage {
  static let answer = 42 // expected-error {{only 'static func' declarations are supported in namespaces}}
}

namespace UnsupportedNestedType {
  struct Payload {} // expected-error {{only 'static func' declarations are supported in namespaces}}
}

namespace NoImplicitSelf {
  static func rejectSelfType() {
    _ = Self.self // expected-error {{cannot find 'Self' in scope}}
  }

  static func rejectSelfValue() {
    _ = self // expected-error {{cannot find 'self' in scope; did you mean to use it in a type or extension context?}}
  }
}
