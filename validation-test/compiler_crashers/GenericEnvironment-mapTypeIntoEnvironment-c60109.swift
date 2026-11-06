// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"26d2079b","signature":"swift::GenericEnvironment::mapTypeIntoEnvironment(swift::GenericEnvironment*, swift::Type)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"createImplicitConstructor"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
struct a {
  @propertyWrapper class b<c {
    wrappedValue: c  init(wrappedValue: c...
  }
  @b var d = 5
