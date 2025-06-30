// {"kind":"complete","signature":"swift::ide::printTypeUSR(swift::Type, llvm::raw_ostream&)"}
// Actual signature: getContextSubstitutions
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
protocol a extension a where Self == { extension a#^COMPLETE^# protocol b
