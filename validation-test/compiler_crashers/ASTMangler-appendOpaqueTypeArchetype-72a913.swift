// {"kind":"typecheck","original":"74cf1b5a","signature":"swift::Mangle::ASTMangler::appendOpaqueTypeArchetype(swift::ArchetypeType*, swift::OpaqueTypeDecl*, swift::SubstitutionMap, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b<c> -> (some a)? where c ==
