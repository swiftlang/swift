// {"extraArgs":["-enable-library-evolution","-O","-g"],"kind":"emit-ir","signature":"(anonymous namespace)::IRGenDebugInfoImpl::getMangledName(swift::irgen::DebugTypeInfo)","signatureNext":"IRGenDebugInfoImpl::getOrCreateType"}
// RUN: %target-swift-frontend -emit-ir -enable-library-evolution -O -g %s
public protocol a {
  associatedtype b
  func c(d: b)
}
public struct e<f>: a {
  public func c(d: f) {
  }
}
