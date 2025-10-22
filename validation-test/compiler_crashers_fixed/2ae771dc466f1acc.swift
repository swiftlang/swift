// {"kind":"typecheck","original":"f0cca09d","signature":"swift::ASTWalker::PostWalkResult<swift::ArrayToPointerExpr*> (anonymous namespace)::Verifier::dispatchVisitPost<swift::ArrayToPointerExpr*>(swift::ArrayToPointerExpr*)"}
// RUN: %target-typecheck-verify-swift
struct a {
  var b: String
}
func c<d>(e: PartialKeyPath<d>, _: UnsafePointer<d> ...) {
  func f(i: UnsafePointer<Int64>) {
    i.withMemoryRebound(to: a.self, capacity: 2) { g in
      for h in 0 ..< 2 {
        var elt = g[h]
        c(e: \.b, &elt)
      }
    }
  }
}
