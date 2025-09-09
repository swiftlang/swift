// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::matchResultBuilder(swift::AnyFunctionRef, swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (builder->getAttrs().hasAttribute<ResultBuilderAttr>()), function matchResultBuilder"}
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
