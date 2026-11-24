// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"16928868","signature":"swift::ParamDecl::getDefaultValueStringRepresentation(llvm::SmallVectorImpl<char>&) const","signatureAssert":"Assertion failed: (DefaultValueAndFlags.getPointer() && \"default value not provided yet\"), function getDefaultValueStringRepresentation","signatureNext":"Serializer::DeclSerializer::visitParamDecl"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
class a {
  init(
    a  =
      #b
  class c: a
