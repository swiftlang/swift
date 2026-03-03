// {"kind":"typecheck","original":"679d3d53","signature":"(anonymous namespace)::PrintAST::visit(swift::Decl*)"}
// RUN: not %target-swift-frontend -typecheck %s
extension () {
  class let a
}
