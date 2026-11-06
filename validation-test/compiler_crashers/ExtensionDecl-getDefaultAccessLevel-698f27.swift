// {"kind":"typecheck","signature":"swift::ExtensionDecl::getDefaultAccessLevel() const","signatureNext":"AccessLevelRequest::OutputType"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b extension a where b : c, d == a {
  protocol c struct a
