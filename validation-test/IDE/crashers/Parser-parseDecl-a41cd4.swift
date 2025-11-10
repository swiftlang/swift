// {"kind":"complete","signature":"swift::Parser::parseDecl(bool, bool, llvm::function_ref<void (swift::Decl*)>, bool)","signatureAssert":"Assertion failed: (!InEnumElementRawValue), function completeNominalMemberBeginning"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
case <#expression#>= { protocol a { #^COMPLETE^#
