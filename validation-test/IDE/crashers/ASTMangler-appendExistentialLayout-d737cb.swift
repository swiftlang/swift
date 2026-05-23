// {"kind":"complete","original":"4ff768bf","signature":"swift::Mangle::ASTMangler::appendExistentialLayout(swift::ExistentialLayout const&, swift::GenericSignature, swift::ValueDecl const*)","stackOverflow":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  typealias b = d
}
protocol c {
  associatedtype b
}
extension a where Self#^^#: c {
  protocol d
