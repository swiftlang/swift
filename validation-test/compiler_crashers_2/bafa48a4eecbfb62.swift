// {"signature":"swift::InFlightDiagnostic swift::DiagnosticEngine::diagnose<unsigned int, unsigned int, bool>(swift::SourceLoc, swift::Diag<unsigned int, unsigned int, bool>, swift::detail::PassArgument<unsigned int>::type, swift::detail::PassArgument<unsigned int>::type, swift::detail::PassArgument<bool>::type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class b open extension b {
  @objc c : a
