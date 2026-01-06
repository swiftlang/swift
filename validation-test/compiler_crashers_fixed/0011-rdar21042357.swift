// RUN: not %target-swift-frontend %s -typecheck

protocol A {}
extension A {
  final func f() {}
protocol B {
  func f()
}
struct S : A, B {}
