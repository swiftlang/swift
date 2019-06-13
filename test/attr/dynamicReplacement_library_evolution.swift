// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-library-evolution  -I %t

struct Container {
  dynamic var property: Int { return 1 } // expected-error{{marking non-'@objc' Swift declaration 'dynamic' in library evolution mode not supported}}
  dynamic func foo() {} // expected-error{{marking non-'@objc' Swift declaration 'dynamic' in library evolution mode not supported}}
}

class AClass {
  dynamic var property: Int { return 1 } // expected-error{{marking non-'@objc' Swift declaration 'dynamic' in library evolution mode not supported}}
  dynamic func foo() {} // expected-error{{marking non-'@objc' Swift declaration 'dynamic' in library evolution mode not supported}}

  @objc dynamic func allowed() {}


  @objc dynamic var allowedProperty : Int { return 1}
}
