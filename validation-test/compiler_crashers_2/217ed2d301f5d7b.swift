// {"kind":"typecheck","original":"679d3d53","signature":"(anonymous namespace)::PrintAST::visit(swift::Decl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension () {
  class let a
}
