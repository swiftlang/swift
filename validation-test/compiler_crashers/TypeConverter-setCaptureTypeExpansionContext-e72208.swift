// {"issueID":85266,"kind":"emit-silgen","signature":"swift::Lowering::TypeConverter::setCaptureTypeExpansionContext(swift::SILDeclRef, swift::SILModule&)","signatureAssert":"Assertion failed: (existing->second == context && \"closure shouldn't be emitted with different capture type expansion contexts\"), function setCaptureTypeExpansionContext"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// https://github.com/swiftlang/swift/issues/85266
func a() {
  func b() -> Any {
    c
  }
  lazy var c = b
}
