// {"kind":"typecheck","signature":"swift::GenericSignatureImpl::lookupNestedType(swift::Type, swift::Identifier) const","signatureNext":"TypeResolver::resolveDeclRefTypeReprRec"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
a.b
protocol c where a.d == Self
  protocol a {
    associatedtype e : c
    typealias b
    typealias d = Self.f
