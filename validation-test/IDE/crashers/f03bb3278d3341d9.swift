// {"kind":"complete","signature":"swift::PersistentParserState::restoreIDEInspectionDelayedDeclState(swift::IDEInspectionDelayedDeclState const&)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
guard {
#if #^COMPLETE^#
0
