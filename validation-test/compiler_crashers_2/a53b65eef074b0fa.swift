// {"signature":"swift::constraints::MissingConformanceFailure::diagnoseTypeCannotConform(swift::Type, swift::Type) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a extension a {
  b { func * (Self= {
