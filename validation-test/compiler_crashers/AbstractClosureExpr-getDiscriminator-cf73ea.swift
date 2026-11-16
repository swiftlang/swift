// {"kind":"emit-silgen","signature":"swift::AbstractClosureExpr::getDiscriminator() const","signatureNext":"Mangle::ASTMangler::appendClosureEntity"}
// RUN: not --crash %target-swift-frontend -emit-silgen -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck %s
@propertyWrapper public struct W {
  public var wrappedValue: Int
  public init(wrappedValue: Int) { self.wrappedValue = wrappedValue }
  public var projectedValue: W { self }
  public init(projectedValue: W) { self = projectedValue }
}
public func f() -> Int {
  return g(x: 1, nil, 2)
}
public func g(@W x: Int, _ a: Int?, _ b: Int) -> Int {
  return a ?? b
}
