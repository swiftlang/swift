// {"kind":"typecheck","original":"cc97dd1d","signature":"(anonymous namespace)::ExprWalker::rewriteTarget(swift::constraints::SyntacticElementTarget)","signatureNext":"SyntacticElementSolutionApplication::apply"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
let a = DispatchQueue(
var lock = NSLock
func b -> AsyncStream <()> {
  AsyncStream { c in
    a.async {
      lock.lock
      if
