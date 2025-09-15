// {"signature":"(anonymous namespace)::Traversal::doIt(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  for b 0 ..< 10 {
    let a = Array(0 ..< b)
    for c d a{
      for e d c... {
        Array(a[c ..< e].reversed())
        f
