// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"00583d9e","signature":"swift::substOpaqueTypesWithUnderlyingTypes(swift::CanType, swift::TypeExpansionContext)","signatureNext":"Lowering::TypeConverter::computeLoweredRValueType::LoweredRValueTypeVisitor::visitType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a
  func b -> some a {
    func c -> some a {
      b(
    }
    return c(
