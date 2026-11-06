// {"kind":"typecheck","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::Type)","signatureNext":"Verifier::dispatchVisitPost"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b {
  d {
    class e<c, b> : a struct f subscript<g>(h : e<f, g>) {
      h.d base
