// {"kind":"typecheck","signature":"(anonymous namespace)::ASTScopeDeclConsumerForUnqualifiedLookup::consume(llvm::ArrayRef<swift::ValueDecl*>, swift::NullablePtr<swift::DeclContext>)","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @abi(func a) var b : c
