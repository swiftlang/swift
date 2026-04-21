// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"79147ddb","signature":"swift::Lowering::AbstractionPattern::forEachExpandedTupleElement(swift::CanType, llvm::function_ref<void (swift::Lowering::AbstractionPattern, swift::CanType, swift::TupleTypeElt const&)>) const","signatureAssert":"Assertion failed: (matchesTuple(substType)), function forEachExpandedTupleElement","signatureNext":"Lowering::TypeConverter::computeLoweredRValueType::LoweredRValueTypeVisitor::visitTupleType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a<each b> {
  case (String, repeat each b)
}
func c() -> a<Int>
