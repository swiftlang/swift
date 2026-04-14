// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"6911c977","signature":"swift::SourceManager::extractText(swift::CharSourceRange, std::__1::optional<unsigned int>) const","signatureAssert":"Assertion failed: (Range.isValid() && \"range should be valid\"), function extractText","signatureNext":"extractInlinableText"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
@propertyWrapper struct a<b> {
  var wrappedValue: b
}
struct c {
  @a var d: ()
}
