// {"kind":"typecheck","original":"54198387","signature":"swift::DeclNameRef::getBaseIdentifier() const","signatureAssert":"Assertion failed: (!baseName.isSpecial() && \"Can't retrieve the identifier of a special base name\"), function getBaseIdentifier","signatureNext":"fixMissingArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(
  c: (b, ()) -> b = {
    $0.init
  })
