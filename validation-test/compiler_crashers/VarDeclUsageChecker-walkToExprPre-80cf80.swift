// {"kind":"typecheck","original":"eed30a35","signature":"(anonymous namespace)::VarDeclUsageChecker::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (AllExprsSeen.insert(E).second && \"duplicate traversal\"), function walkToExprPre","signatureNext":"Traversal::doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
DispatchQueue.global(qos: .userInitiated).async {
  DispatchQueue.global
  a
}
