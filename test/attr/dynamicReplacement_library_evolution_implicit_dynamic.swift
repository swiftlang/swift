// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -enable-objc-interop -enable-library-evolution -enable-implicit-dynamic -I %t

struct Container {
  var property: Int { return 1 }
  func foo() {}
}

class AClass {
  var property: Int { return 1 }
  func foo() {}

  @objc dynamic func allowed() {}


  @objc dynamic var allowedProperty : Int { return 1}
}
