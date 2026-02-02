// {"kind":"typecheck","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  extension {
    a {
      func b {
        super
