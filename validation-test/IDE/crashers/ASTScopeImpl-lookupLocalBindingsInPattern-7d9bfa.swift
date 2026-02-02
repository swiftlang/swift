// {"kind":"complete","original":"831da109","signature":"void llvm::function_ref<void (swift::VarDecl*)>::callback_fn<swift::ast_scope::ASTScopeImpl::lookupLocalBindingsInPattern(swift::Pattern const*, swift::namelookup::AbstractASTScopeDeclConsumer&)::$_0>(long, swift::VarDecl*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  @abi(func & ()) var a {
    #^^#
  }
}
