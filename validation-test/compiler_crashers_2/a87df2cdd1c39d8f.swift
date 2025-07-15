// {"kind":"typecheck","signature":"diagSyntacticUseRestrictions(swift::Expr const*, swift::DeclContext const*, bool)::DiagnoseWalker::checkUseOfMetaTypeName(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a : Equatable, ExpressibleByStringLiteral {
}
{
  struct b: a {
    static var c: b
  }
  [ b.c == "" , b]
}
