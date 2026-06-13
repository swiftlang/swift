// {"kind":"typecheck","original":"2789707c","signature":"swift::Mangle::ASTMangler::appendOpaqueTypeArchetype(swift::ArchetypeType*, swift::OpaqueTypeDecl*, swift::SubstitutionMap, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendBoundGenericArgs"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b<c> -> Result<some a, Error> where c ==
