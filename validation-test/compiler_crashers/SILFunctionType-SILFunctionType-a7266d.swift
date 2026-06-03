// {"kind":"emit-silgen","original":"35e9fe61","signature":"swift::SILFunctionType::SILFunctionType(swift::GenericSignature, swift::SILExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, llvm::ArrayRef<swift::SILParameterInfo>, llvm::ArrayRef<swift::SILYieldInfo>, llvm::ArrayRef<swift::SILResultInfo>, std::__1::optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, swift::ASTContext const&, swift::RecursiveTypeProperties, swift::ProtocolConformanceRef)","signatureAssert":"Assertion failed: (!param.getInterfaceType()->hasArchetype() && \"interface type of parameter should not contain context archetypes\"), function SILFunctionType","signatureNext":"SILFunctionType::get"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@dynamicMemberLookup
struct a<b> {
  subscript<c>(dynamicMember d: KeyPath<b, c>) -> e<b, c> {
  }
}
struct e<f, c> {
}
protocol g {
  associatedtype h
  func i<j>(
    k: (a<h>) -> j
  )
}
@available(SwiftStdlib 5.9, *)
struct l<each m: Hashable> {
  func p<each j: g>(
    n: repeat each j
  ) where (repeat each m) == (repeat (each j).h) {
    repeat (each n).i { o in
      o.hashValue
    }
  }
}
