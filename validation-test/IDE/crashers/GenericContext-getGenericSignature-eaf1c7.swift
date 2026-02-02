// {"kind":"complete","original":"2aa5adcf","signature":"swift::GenericContext::getGenericSignature() const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b> : Collection }
a.Iterator.c #^^#
