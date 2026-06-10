// {"frontendArgs":["-typecheck"],"kind":"custom","signature":"swift::AvailabilityScope::verify(swift::AvailabilityScope const*, swift::ASTContext&) const","signatureNext":"verify"}
// RUN: not %target-swift-frontend -typecheck %s
switch  {
case :
  let a
