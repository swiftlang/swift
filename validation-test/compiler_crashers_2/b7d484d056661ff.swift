// {"signature":"swift::PackExpansionType::PackExpansionType(swift::Type, swift::Type, swift::RecursiveTypeProperties, swift::ASTContext const*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[Int : Int](Int) { a, b in a[b b= b * b
