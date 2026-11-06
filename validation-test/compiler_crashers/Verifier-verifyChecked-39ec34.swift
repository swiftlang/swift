// {"kind":"typecheck","original":"0ba9e16b","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::Type)","signatureNext":"Verifier::walkToPatternPost"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func c  throws(b
}
func d<each e: a>(f: repeat each e) {
  do {
    repeat (each f).c(
  } catch
