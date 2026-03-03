// {"kind":"typecheck","original":"71a26232","signature":"swift::Decl::getDescriptiveKind() const"}
// RUN: not %target-swift-frontend -typecheck %s
typealias a = ()
extension a {
  func
    < (b: Self, c: Self)
  {
    for (d, e) in repeat (each b    each c) {
      d < &e
    }
  }
}
