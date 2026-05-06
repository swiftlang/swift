// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"506d0758","signature":"swift::Lowering::TypeConverter::getTypeLowering(swift::Lowering::AbstractionPattern, swift::Type, swift::TypeExpansionContext)","signatureAssert":"Assertion failed: (false), function verifyTrivialLowering","signatureNext":"SILType::getFieldType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol b: AnyObject
  @propertyWrapper struct g<c: b {
    weak  d: c?
    var wrappedValue: c?
  }
  struct e: b
    struct f {
      @g  a: e?
