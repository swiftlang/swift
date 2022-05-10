// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

class Base<T> {}
class Derived : Base<Int> {}

struct Holder<A : Base<B>, B> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Holder
// CHECK-NEXT: Generic signature: <A, B where A : Derived, B == Int>
extension Holder where A : Derived {

  // Make sure that 'B == Int' is implied by 'A : Base<B>' together with
  // 'A : Derived', since 'Derived : Base<Int>'.
  func returnInt(_ b: B) -> Int { return b }
}
