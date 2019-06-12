// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-library-evolution  -I %t

struct Container {
  dynamic var property: Int { return 1 } // expected-error{{a native Swift declaration cannot be marked 'dynamic' in library evolution mode}}
  dynamic func foo() {} // expected-error{{a native Swift declaration cannot be marked 'dynamic' in library evolution mode}}
}

class AClass {
  dynamic var property: Int { return 1 } // expected-error{{a native Swift declaration cannot be marked 'dynamic' in library evolution mode}}
  dynamic func foo() {} // expected-error{{a native Swift declaration cannot be marked 'dynamic' in library evolution mode}}

  @objc dynamic func allowed() {}


  @objc dynamic var allowedProperty : Int { return 1}
}
