// RUN: not %target-swift-frontend %s -parse

protocol A {}
extension A {
  final func f() {}
protocol B {
  func f()
}
struct S : A, B {}
