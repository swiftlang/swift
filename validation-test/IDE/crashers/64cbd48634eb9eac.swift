// {"kind":"complete","signature":"swift::irgen::IRGenModule::emitLazyObjCProtocolDefinition(swift::ProtocolDecl*)"}
// Actual signature: matchCallArguments
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
{ switch { case let c(#^COMPLETE^# b) a
