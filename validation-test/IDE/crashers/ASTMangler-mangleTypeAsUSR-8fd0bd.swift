// {"kind":"complete","original":"9040bf6c","signature":"swift::Mangle::ASTMangler::mangleTypeAsUSR(swift::Type)","signatureNext":"printTypeUSR"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
}
func c<each d: a>(repeat each d, _: (repeat each d.b?
let e = c(1, #^^#
