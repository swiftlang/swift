// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -primary-file %s -primary-file %S/Inputs/opaque_result_type_private_underlying_2.swift

public struct G2 : A {
  public init() {}
   public func bindAssoc() -> some Q {
     return G().bindAssoc()
   }
}
