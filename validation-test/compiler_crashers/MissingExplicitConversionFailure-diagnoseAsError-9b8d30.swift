// {"kind":"typecheck","original":"28423040","signature":"swift::constraints::MissingExplicitConversionFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ForceDowncast::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
{
  @propertyWrapper struct a {
    var wrappedValue: String
  }
  @a var b: NSString
}
