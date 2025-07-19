// {"signature":"swift::InFlightDiagnostic swift::DiagnosticEngine::diagnose<swift::Type>(swift::Decl const*, swift::Diag<swift::Type>, swift::detail::PassArgument<swift::Type>::type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objcMembers class a open extension a {
  func 0.0
