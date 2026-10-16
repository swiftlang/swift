// {"kind":"emit-silgen","original":"5be7fe62","signature":"swift::SILFunction::getDynamicSelfMetadata() const","signatureAssert":"Assertion failed: (hasDynamicSelfMetadata() && \"This method can only be called if the \" \"SILFunction has a self-metadata parameter\"), function getDynamicSelfMetadata","signatureNext":"buildSILFunctionThunkType"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  func b(c: (Self) -> Void) -> Self
}
class d: a {
  func b(c: (Self) -> Void) -> Self {
  }
}
