// {"kind":"typecheck","original":"d852530c","signature":"(anonymous namespace)::SyntacticElementSolutionApplication::apply()","signatureNext":"ConstraintSystem::applySolutionToSingleValueStmt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
func a(b: Int) -> String {
  withCheckedThrowingContinuation { c in
    let d = DispatchQueue().async {
      if b < 0 {
        c.resume
      } else {
        e
      }
    }
  }
}
