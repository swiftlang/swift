// {"signature":"(anonymous namespace)::Verifier::verifyChecked(swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b {
  d {
    class e<c, b> : a struct f subscript<g>(h : e<f, g>) {
      h.d base
