// {"kind":"typecheck","original":"6bf5a596","signature":"(anonymous namespace)::DeclChecker::visit(swift::Decl*)","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"DeclChecker::visitExtensionDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@implementation
extension
  CInt
{
  deinit {
  }
}
