// {"kind":"typecheck","original":"2e264f29","signature":"swift::NominalType::get(swift::NominalTypeDecl*, swift::Type, llvm::ArrayRef<swift::Type>, swift::ASTContext const&, llvm::ArrayRef<swift::ScopeRef>)","signatureAssert":"Assertion failed: ((!parent || parent->is<NominalType>() || parent->is<UnboundGenericType>()) && \"parent must be a nominal type\"), function get","signatureNext":"TypeChecker::substMemberTypeWithBase"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  protocol b
    struct c
      extension b where Self: a {        d->c
