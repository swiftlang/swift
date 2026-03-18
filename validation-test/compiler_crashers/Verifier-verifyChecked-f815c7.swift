// {"kind":"typecheck","original":"0ba9e16b","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::Type)","signatureNext":"Verifier::verifyCheckedAlways"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func c  throws(b
}
func d<each e: a>(f: repeat each e) {
  do {
    repeat (each f).c(
  } catch {
