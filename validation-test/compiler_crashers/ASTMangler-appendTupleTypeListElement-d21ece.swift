// {"kind":"typecheck","original":"a90e8390","signature":"swift::Mangle::ASTMangler::appendTupleTypeListElement(swift::Identifier, swift::Type, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendTypeList"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b < c where c == {
    func d -> (a, some a
