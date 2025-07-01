// {"kind":"typecheck","signature":"swift::TypeChecker::checkProtocolSelfRequirements(swift::ValueDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func c () where b : AnyObject
}
