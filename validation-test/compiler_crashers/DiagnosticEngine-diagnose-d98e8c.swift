// {"aliases":["swift::DiagnosticState::determineBehavior(swift::Diagnostic const&, swift::SourceManager&) const"],"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"ede44338","signature":"swift::InFlightDiagnostic swift::DiagnosticEngine::diagnose<swift::Type, swift::Type>(swift::SourceLoc, swift::Diag<swift::Type, swift::Type>, swift::detail::PassArgument<swift::Type>::type, swift::detail::PassArgument<swift::Type>::type)","signatureNext":"ContextualFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
// REQUIRES: OS=macosx

// Temporarily disabled while we investigate why it's suddenly not crashing.
// REQUIRES: rdar179474757
class a {
  func
    b(c: a) -> a
  let <#pattern#>: ((a) -> a).Type = type(b)
}
