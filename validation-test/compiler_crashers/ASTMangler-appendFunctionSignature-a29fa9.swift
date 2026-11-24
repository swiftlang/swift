// {"kind":"typecheck","original":"c1c5d27c","signature":"swift::Mangle::ASTMangler::appendFunctionSignature(swift::AnyFunctionType*, swift::GenericSignature, swift::ValueDecl const*, swift::Mangle::ASTMangler::FunctionManglingKind, bool)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendFunctionType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  protocol b
    func c<d> -> some b where d ==
