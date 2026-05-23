// {"kind":"typecheck","original":"b4838a88","signature":"(anonymous namespace)::ExprWalker::rewriteTarget(swift::constraints::SyntacticElementTarget)","signatureNext":"SyntacticElementSolutionApplication::visitBraceElement"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
class a {
  var lock = NSLock
  init() {
    [
      DispatchQueue().async {
        lock.lock
        let b
      }
    ]
  }
}
