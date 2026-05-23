// {"kind":"typecheck","original":"0a63cb20","signature":"swift::constraints::ConstraintSystem::setType(swift::ASTNode, swift::Type, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (type && \"Expected non-null type\"), function setType","signatureNext":"ConstraintSystem::resolveClosure"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  var wrappedValue: <#type#> {
    @propertyWrapper struct b<c> {
      var wrappedValue: c {
        {
          (@b @a ) in
        }
      }
    }
  }
}
