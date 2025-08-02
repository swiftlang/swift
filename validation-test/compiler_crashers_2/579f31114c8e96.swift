// {"kind":"typecheck","signature":"swift::GenericTypeParamDecl::getSourceRange() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { associatedtype b }
public
struct c < d : a extension c {
public
  typealias e = d.b
