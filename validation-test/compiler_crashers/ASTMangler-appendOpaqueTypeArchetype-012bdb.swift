// {"kind":"typecheck","original":"32f433f2","signature":"swift::Mangle::ASTMangler::appendOpaqueTypeArchetype(swift::ArchetypeType*, swift::OpaqueTypeDecl*, swift::SubstitutionMap, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendDeclType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c: a {
  protocol d: c where e == b {
    extension d where b: Hashable {
      var f: some Hashable
    }
  }
}
