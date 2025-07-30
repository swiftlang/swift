// {"kind":"complete","original":"bae855bf","signature":"swift::InFlightDiagnostic swift::DiagnosticEngine::diagnose<swift::Identifier, swift::NominalTypeDecl const*>(swift::Decl const*, swift::Diag<swift::Identifier, swift::NominalTypeDecl const*>, swift::detail::PassArgument<swift::Identifier>::type, swift::detail::PassArgument<swift::NominalTypeDecl const*>::type)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a<b: c, b extension a where b #^^#
