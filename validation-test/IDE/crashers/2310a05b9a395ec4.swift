// {"kind":"complete","signature":"swift::ide::AfterPoundExprCompletion::sawSolutionImpl(swift::constraints::Solution const&)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
{##^COMPLETE^#
