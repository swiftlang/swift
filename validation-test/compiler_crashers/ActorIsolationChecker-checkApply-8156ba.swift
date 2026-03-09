// {"kind":"typecheck","original":"b281d585","signature":"(anonymous namespace)::ActorIsolationChecker::checkApply(swift::ApplyExpr*)","signatureAssert":"Assertion failed: (!IsolationCrossing.has_value() && \"IsolationCrossing should not be set twice\"), function setIsolationCrossing"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a@globalActor actor b {
  static var shared = {
    extension a {
      c { @b func d func e { d(
          }
        }
    }
