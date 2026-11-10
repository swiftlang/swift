// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"26d2079b","signature":"swift::GenericEnvironment::mapTypeIntoEnvironment(swift::GenericEnvironment*, swift::Type)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"createImplicitConstructor"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
struct a {
  @propertyWrapper class b<c {
    wrappedValue: c  init(wrappedValue: c...
  }
  @b var d = 5
