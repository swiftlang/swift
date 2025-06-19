// {"signature":"swift::TypeBase::getNominalParent()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  typealias b<c> = () extension b
