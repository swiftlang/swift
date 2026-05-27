// {"kind":"typecheck","original":"3316325f","signature":"(anonymous namespace)::Traversal::doIt(swift::Expr*)","signatureNext":"Expr::walk"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(b: [UInt8], c: () -> Int) {
  let d = c()
  e
  {
    case: let f = stride(from: 0 to: b.count, by: d).map { b[$0...].map { String($0 } .joined }
    g .<#expression#>
  }
}
