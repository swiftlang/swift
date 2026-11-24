// {"kind":"typecheck","original":"54198387","signature":"swift::DeclNameRef::getBaseIdentifier() const","signatureAssert":"Assertion failed: (!isSpecial() && \"Cannot retrieve identifier from special names\"), function getIdentifier","signatureNext":"fixMissingArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(
  c: (b, ()) -> b = {
    $0.init
  })
