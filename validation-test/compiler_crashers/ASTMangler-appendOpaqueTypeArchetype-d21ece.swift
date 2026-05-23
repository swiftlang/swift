// {"kind":"typecheck","original":"a90e8390","signature":"swift::Mangle::ASTMangler::appendOpaqueTypeArchetype(swift::ArchetypeType*, swift::OpaqueTypeDecl*, swift::SubstitutionMap, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendTupleTypeListElement"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b < c where c == {
    func d -> (a, some a
