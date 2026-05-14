// {"kind":"emit-silgen","original":"6deeffa3","signature":"swift::Lowering::SILGenFunction::emitThrow(swift::SILLocation, swift::Lowering::ManagedValue, bool)","signatureAssert":"Assertion failed: (destErrorType == SILType::getExceptionType(getASTContext())), function emitThrow","signatureNext":"PatternMatchEmission::emitIsDispatch"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a: Error {
}
protocol b {
  associatedtype c
  associatedtype d: a
  func j(_: c) throws(d)
}
struct e<f: b> {
  typealias c = f.c
  enum g: a {
  }
  let h: f
  func j(i: c) throws(g) {
    do {
      try h.j(i)
    } catch let error as f.d {
    }
  }
}
