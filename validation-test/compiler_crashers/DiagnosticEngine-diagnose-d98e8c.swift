// {"aliases":["swift::DiagnosticState::determineBehavior(swift::Diagnostic const&, swift::SourceManager&) const"],"kind":"typecheck","languageMode":6,"original":"ede44338","signature":"swift::InFlightDiagnostic swift::DiagnosticEngine::diagnose<swift::Type, swift::Type>(swift::SourceLoc, swift::Diag<swift::Type, swift::Type>, swift::detail::PassArgument<swift::Type>::type, swift::detail::PassArgument<swift::Type>::type)","signatureNext":"ContextualFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck -swift-version 6 %s
class a {
  func
    b(c: a) -> a
  let <#pattern#>: ((a) -> a).Type = type(b)
}
