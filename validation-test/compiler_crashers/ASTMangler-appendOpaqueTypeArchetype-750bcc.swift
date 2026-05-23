// {"kind":"typecheck","original":"7e87f396","signature":"swift::Mangle::ASTMangler::appendOpaqueTypeArchetype(swift::ArchetypeType*, swift::OpaqueTypeDecl*, swift::SubstitutionMap, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendFunctionSignature"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b<c> -> some a where c
=
