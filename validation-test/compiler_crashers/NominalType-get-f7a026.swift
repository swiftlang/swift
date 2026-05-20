// {"kind":"typecheck","original":"2e264f29","signature":"swift::NominalType::get(swift::NominalTypeDecl*, swift::Type, swift::ASTContext const&)","signatureAssert":"Assertion failed: ((!Parent || Parent->is<NominalType>() || Parent->is<BoundGenericType>() || Parent->is<UnboundGenericType>()) && \"parent must be a nominal type\"), function get","signatureNext":"TypeChecker::substMemberTypeWithBase"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  protocol b
    struct c
      extension b where Self: a {        d->c
