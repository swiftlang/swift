// {"kind":"complete","signature":"swift::ide::AfterPoundExprCompletion::sawSolutionImpl(swift::constraints::Solution const&)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{##^COMPLETE^#
