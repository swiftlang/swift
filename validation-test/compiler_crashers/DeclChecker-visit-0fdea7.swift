// {"kind":"typecheck","original":"6bf5a596","signature":"(anonymous namespace)::DeclChecker::visit(swift::Decl*)","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@implementation
extension
  CInt
{
  deinit {
  }
}
