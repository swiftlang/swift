// {"signature":"(anonymous namespace)::DeclChecker::visitExtensionDecl(swift::ExtensionDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
extension repeat (
