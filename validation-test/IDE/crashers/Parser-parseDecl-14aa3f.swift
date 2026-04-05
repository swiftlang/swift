// {"kind":"complete","original":"24d79ca6","signature":"swift::Parser::parseDecl(bool, bool, llvm::function_ref<void (swift::Decl*)>, bool)","signatureAssert":"Assertion failed: (!InEnumElementRawValue), function completeNominalMemberBeginning","signatureNext":"Parser::parseBraceItems"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
case = { static #^^#<#expression#>
