// {"kind":"typecheck","original":"a90e8390","signature":"swift::Mangle::ASTMangler::appendTupleTypeListElement(swift::Identifier, swift::Type, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (subs.isIdentity()), function appendOpaqueTypeArchetype","signatureNext":"Mangle::ASTMangler::appendTypeList"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b < c where c == {
    func d -> (a, some a
