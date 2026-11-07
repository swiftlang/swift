// {"kind":"typecheck","original":"0fb58417","signature":"swift::GenericEnvironment::mapElementTypeIntoPackContext(swift::Type) const","signatureAssert":"Assertion failed: (type->hasElementArchetype()), function mapElementTypeIntoPackContext","signatureNext":"ExprRewriter::coerceToType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b {
  typealias c = Int
  func f -> Int
  func d<each e> -> (repeat a<each e>.c?) {
    (repeat a<each e>().f(
