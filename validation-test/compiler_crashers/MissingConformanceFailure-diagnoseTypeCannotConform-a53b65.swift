// {"kind":"typecheck","signature":"swift::constraints::MissingConformanceFailure::diagnoseTypeCannotConform(swift::Type, swift::Type) const","signatureNext":"MissingConformanceFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a extension a {
  b { func * (Self= {
