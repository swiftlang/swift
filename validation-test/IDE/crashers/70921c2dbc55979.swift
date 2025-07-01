// {"kind":"complete","signature":"swift::irgen::IRGenModule::emitLazyObjCProtocolDefinition(swift::ProtocolDecl*)"}
// Actual signature: matchCallArguments
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
a(
#^COMPLETE^#[
""!
