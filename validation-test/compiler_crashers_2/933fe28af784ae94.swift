// {"signature":"(anonymous namespace)::ASTScopeDeclConsumerForUnqualifiedLookup::consume(llvm::ArrayRef<swift::ValueDecl*>, swift::NullablePtr<swift::DeclContext>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @abi(func a) var b : c
