// {"signature":"swift::ExtensionDecl::getExtendedNominal() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension a {}
func b < >>
