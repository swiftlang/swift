// {"kind":"complete","signature":"swift::ide::AfterPoundExprCompletion::sawSolutionImpl(swift::constraints::Solution const&)"}
// RUN: %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
a ##^COMPLETE^# / ( b/bin/c
