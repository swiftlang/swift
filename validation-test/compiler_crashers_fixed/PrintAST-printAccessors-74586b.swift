// {"kind":"typecheck","original":"29a2d7da","signature":"(anonymous namespace)::PrintAST::printAccessors(swift::AbstractStorageDecl const*)"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  enum b: a
    enum c: a
      let d: a
