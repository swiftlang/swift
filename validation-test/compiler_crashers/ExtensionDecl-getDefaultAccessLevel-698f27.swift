// {"kind":"typecheck","signature":"swift::ExtensionDecl::getDefaultAccessLevel() const","signatureNext":"AccessLevelRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b extension a where b : c, d == a {
  protocol c struct a
