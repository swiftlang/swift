// {"kind":"typecheck","original":"7e87f396","signature":"swift::Mangle::ASTMangler::appendFunctionSignature(swift::AnyFunctionType*, swift::GenericSignature, swift::ValueDecl const*, swift::Mangle::ASTMangler::FunctionManglingKind, bool)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendDeclType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b<c> -> some a where c
=
