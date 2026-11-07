// {"kind":"typecheck","signature":"swift::constraints::MissingConformanceFailure::diagnoseTypeCannotConform(swift::Type, swift::Type) const","signatureNext":"MissingConformanceFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a extension a {
  b { func * (Self= {
