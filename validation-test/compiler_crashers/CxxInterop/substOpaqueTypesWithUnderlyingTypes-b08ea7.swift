// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"491d702f","signature":"swift::substOpaqueTypesWithUnderlyingTypes(swift::CanType, swift::TypeExpansionContext)","signatureNext":"Lowering::TypeConverter::computeLoweredRValueType::LoweredRValueTypeVisitor::visitType"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
func a() -> some Any {
  a
}
