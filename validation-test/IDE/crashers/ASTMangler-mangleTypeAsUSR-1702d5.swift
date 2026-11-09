// {"kind":"complete","original":"0124fd75","signature":"swift::Mangle::ASTMangler::mangleTypeAsUSR(swift::Type)","signatureNext":"printTypeUSR"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
  associatedtype c
}
func d<e: a>( [e.b] -> [e.c] = d(#^^#
