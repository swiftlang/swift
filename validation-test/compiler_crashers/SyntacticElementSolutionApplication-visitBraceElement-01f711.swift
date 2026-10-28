// {"kind":"typecheck","original":"52a144cd","signature":"(anonymous namespace)::SyntacticElementSolutionApplication::visitBraceElement(swift::ASTNode)","signatureNext":"SyntacticElementSolutionApplication::apply"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
{ (a: CheckedContinuation<Bool, Never>) in
  DispatchQueue().async {
    a.resume
    b
  }
}
