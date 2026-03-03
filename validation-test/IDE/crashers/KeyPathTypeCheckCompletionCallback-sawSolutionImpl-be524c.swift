// {"kind":"complete","original":"1433c7e3","signature":"swift::ide::KeyPathTypeCheckCompletionCallback::sawSolutionImpl(swift::constraints::Solution const&)","signatureAssert":"Assertion failed: (ComponentIndex < KeyPath->getComponents().size() && \"Didn't find a code compleiton component?\"), function sawSolutionImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{ \a#^^# ? }
<
