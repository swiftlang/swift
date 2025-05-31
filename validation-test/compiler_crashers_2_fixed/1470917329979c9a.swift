// {"signature":"swift::PackExpansionType::PackExpansionType(swift::Type, swift::Type, swift::RecursiveTypeProperties, swift::ASTContext const*)"}
// RUN: not %target-swift-frontend -typecheck %s
struct a < each b {
  struct c {
          d : (repeat(each b
  func e {
          c(&0
