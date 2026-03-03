// {"kind":"complete","signature":"swift::ParserStatus llvm::function_ref<swift::ParserStatus ()>::callback_fn<swift::Parser::parseParameterClause(swift::SourceLoc&, llvm::SmallVectorImpl<swift::Parser::ParsedParameter>&, swift::SourceLoc&, swift::Parser::DefaultArgumentInfo*, swift::Parser::ParameterContextKind)::$_0>(long)","signatureAssert":"Assertion failed: (getPtrOrNull() && \"not checked for nullptr\"), function get"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func 丏(=1 as
#^COMPLETE^#
