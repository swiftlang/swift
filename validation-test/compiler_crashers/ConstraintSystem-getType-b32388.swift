// {"kind":"typecheck","original":"4b05db17","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"FailureDiagnostic::getType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a {
  case b
  enum c  ; d(e: c??) {
    + switch e { case .some(.some(carriersome(b
      }
  }
