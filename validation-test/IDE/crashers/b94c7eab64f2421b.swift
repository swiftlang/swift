// {"kind":"complete","original":"66e1ef62","signature":"swift::Mangle::ASTMangler::appendConstrainedExistential(swift::Type, swift::GenericSignature, swift::ValueDecl const*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b, c> {
  associatedtype b
  associatedtype c
}
var b {
  #^^#
}
func d() -> a<a, some a>
