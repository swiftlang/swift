// {"kind":"complete","original":"6bce2476","signature":"swift::ide::PostfixCompletionCallback::sawSolutionImpl(swift::constraints::Solution const&)","signatureAssert":"Assertion failed: (BaseIsStaticMetaType == Other.BaseIsStaticMetaType), function tryMerge"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension BinaryInteger {
a { .Iterator.Element#^^# ==
self
