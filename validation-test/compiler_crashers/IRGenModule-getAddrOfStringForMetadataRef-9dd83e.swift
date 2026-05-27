// {"kind":"emit-ir","original":"1c541058","signature":"swift::irgen::IRGenModule::getAddrOfStringForMetadataRef(llvm::StringRef, unsigned int, bool, llvm::function_ref<clang::CodeGen::ConstantInitFuture (swift::irgen::ConstantInitBuilder&)>)","signatureAssert":"Assertion failed: (metadata.GenericPackArguments.empty() && \"We don't support packs here yet\"), function operator()","signatureNext":"IRGenModule::getAddrOfGenericEnvironment"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
func a<b, each c>(
  d: b,
  e: repeat KeyPath<b, each c>
) -> (b, repeat each c) {
  let f = \b.self
  let g = d
  return (g, repeat d[keyPath: each e])
}
