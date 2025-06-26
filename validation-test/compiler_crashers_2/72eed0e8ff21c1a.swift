// {"signature":"swift::InFlightDiagnostic swift::diagnoseAttrWithRemovalFixIt<swift::Diag<swift::AccessorDecl const*>&, swift::AccessorDecl const*&>(swift::Decl const*, swift::DeclAttribute const*, swift::Diag<swift::AccessorDecl const*>&, swift::AccessorDecl const*&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc b(_)
