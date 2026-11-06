// {"extraArgs":["-language-mode","6","-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"467f6a52","signature":"swift::Lowering::TypeConverter::verifyTrivialLowering(swift::Lowering::TypeLowering const&, swift::Lowering::AbstractionPattern, swift::CanType, swift::TypeExpansionContext)","signatureAssert":"Assertion failed: (false), function verifyTrivialLowering","signatureNext":"Lowering::TypeConverter::getTypeLowering"}
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
@propertyWrapper struct a<b: Sendable> {
  var wrappedValue: b {
  }
}
@propertyWrapper struct f<c> {
  var wrappedValue: c
}
struct d {
  @a @f var e: Int
}
