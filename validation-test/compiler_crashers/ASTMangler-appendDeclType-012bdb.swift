// {"kind":"typecheck","original":"32f433f2","signature":"swift::Mangle::ASTMangler::appendDeclType(swift::ValueDecl const*, swift::Mangle::ASTMangler::BaseEntitySignature&, swift::Mangle::ASTMangler::FunctionManglingKind)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendAccessorEntity"}
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
