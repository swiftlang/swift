// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::matchResultBuilder(swift::AnyFunctionRef, swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (builder->getAttrs().hasAttribute<ResultBuilderAttr>()), function matchResultBuilder","signatureNext":"ConstraintSystem::resolveClosure"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  @resultBuilder struct b
  }
  struct c < d {
    init(@b content : () -> d
  }
  extension c : a
    struct b {
e {
  c {
