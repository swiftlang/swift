// {"kind":"typecheck","original":"a290b075","signature":"(anonymous namespace)::ASTScopeDeclConsumerForLocalLookup::consume(llvm::ArrayRef<swift::ValueDecl*>, swift::NullablePtr<swift::DeclContext>)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"ASTScopeImpl::lookupLocalBindingsInPattern"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @abi(func & ) var a  (b
