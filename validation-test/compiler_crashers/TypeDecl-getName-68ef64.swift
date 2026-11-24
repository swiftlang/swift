// {"kind":"typecheck","original":"3814c510","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (getFromType()->is<AnyFunctionType>() && getToType()->is<AnyFunctionType>()), function FunctionTypeMismatch"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension {
  a < b {
    enum c {
      case d(Int, e : (
    }
    func f -> (() (), (), (Int (b)) -> c) {
      (i
      g, h, c.d
