// {"kind":"typecheck","signature":"swift::GenericTypeParamDecl::getSourceRange() const","signatureAssert":"Assertion failed: (Start.isValid() == End.isValid() && \"Start and end should either both be valid or both be invalid!\"), function SourceRange"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { associatedtype b }
public
struct c < d : a extension c {
public
  typealias e = d.b
