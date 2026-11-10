// {"kind":"typecheck","original":"a6956489","signature":"swift::PackType::getSingletonPackExpansion(swift::Type)","signatureAssert":"Assertion failed: (rootParameterPacks.size() >= 1), function getSingletonPackExpansion","signatureNext":"ConstraintSystem::openGenericParameters"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c where b == c
}
protocol d: a where c == [b]
  func e<f, each h> -> some a where f: d {
    g
