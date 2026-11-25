// {"extraArgs":["-enable-experimental-feature","TupleConformances"],"kind":"emit-ir","signature":"swift::Mangle::ASTMangler::appendProtocolConformanceRef(swift::RootProtocolConformance const*)","signatureNext":"Mangle::ASTMangler::appendConcreteProtocolConformance"}
// RUN: not --crash %target-swift-frontend -emit-ir -enable-experimental-feature TupleConformances %s
// REQUIRES: swift_feature_TupleConformances
public protocol a {
  associatedtype b
}
typealias c<each d> = (repeat each d)
extension c: a where repeat each d: a {
  public typealias b = (repeat (each d).b)
  func e() -> some a {
    ()
  }
}
