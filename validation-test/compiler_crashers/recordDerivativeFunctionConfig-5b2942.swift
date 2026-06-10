// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"9d2758c6","signature":"recordDerivativeFunctionConfig(swift::serialization::Serializer&, swift::AbstractFunctionDecl const*, llvm::MapVector<swift::Identifier, llvm::SetVector<std::__1::pair<swift::Identifier, swift::GenericSignature>, llvm::SmallVector<std::__1::pair<swift::Identifier, swift::GenericSignature>, 4u>, llvm::SmallDenseSet<std::__1::pair<swift::Identifier, swift::GenericSignature>, 4u, llvm::DenseMapInfo<std::__1::pair<swift::Identifier, swift::GenericSignature>, void>>, 0u>, llvm::DenseMap<swift::Identifier, unsigned int, llvm::DenseMapInfo<swift::Identifier, void>, llvm::detail::DenseMapPair<swift::Identifier, unsigned int>>, llvm::SmallVector<std::__1::pair<swift::Identifier, llvm::SetVector<std::__1::pair<swift::Identifier, swift::GenericSignature>, llvm::SmallVector<std::__1::pair<swift::Identifier, swift::GenericSignature>, 4u>, llvm::SmallDenseSet<std::__1::pair<swift::Identifier, swift::GenericSignature>, 4u, llvm::DenseMapInfo<std::__1::pair<swift::Identifier, swift::GenericSignature>, void>>, 0u>>, 0u>>&)","signatureNext":"collectInterestingNestedDeclarations"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
{
  struct a {
    @differentiable()<#declaration#>
    b
  }
  &
}
