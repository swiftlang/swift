// {"kind":"typecheck","original":"0fb58417","signature":"swift::GenericEnvironment::mapElementTypeIntoPackContext(swift::Type) const","signatureAssert":"Assertion failed: (type->hasElementArchetype()), function mapElementTypeIntoPackContext","signatureNext":"ExprRewriter::coerceToType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b {
  typealias c = Int
  func f -> Int
  func d<each e> -> (repeat a<each e>.c?) {
    (repeat a<each e>().f(
