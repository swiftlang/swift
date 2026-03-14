// {"kind":"emit-silgen","languageMode":6,"original":"8b10024f","signature":"swift::collectExistentialConformances(swift::CanType, swift::CanType, bool)","signatureAssert":"Assertion failed: (conformance), function collectExistentialConformances"}
// RUN: not --crash %target-swift-frontend -emit-silgen -swift-version 6 %s
protocol a {
  static func b(c: AnyObject)
}
func d<f, h>(_: f, _: (f) -> h) {
}
class g<f: a> {
  func e(c: AnyObject) {
    d(c, f.b)
  }
}
