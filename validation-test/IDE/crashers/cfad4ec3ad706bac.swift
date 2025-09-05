// {"kind":"complete","original":"b353d673","signature":"swift::ide::ArgumentTypeCheckCompletionCallback::sawSolutionImpl(swift::constraints::Solution const&)","signatureAssert":"Assertion failed: (false && \"bad parent call match?\"), function sawSolutionImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  b ? \.a  (#^^#
}(
