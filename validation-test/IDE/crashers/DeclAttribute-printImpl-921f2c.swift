// {"kind":"complete","original":"6da15c2a","signature":"swift::DeclAttribute::printImpl(swift::ASTPrinter&, swift::PrintOptions const&, swift::Decl const*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
associatedtype b func c(d: b) enum e: a {
subscript -> f
#^^#@_implements(a, c(d:)) func g
