// {"kind":"typecheck","original":"a9ab2a5f","signature":"swift::constraints::ConstraintSystem::setType(swift::ASTNode, swift::Type, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (!node.isNull() && \"Cannot set type information on null node\"), function setType","signatureNext":"ConstraintSystem::applyPropertyWrapperToParameter"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  init(wrappedValue: b)
  var wrappedValue: b {
    mutating get
  }
  var projectedValue  init(projectedValue
  c
  struct b {
      d(@a b  = d
