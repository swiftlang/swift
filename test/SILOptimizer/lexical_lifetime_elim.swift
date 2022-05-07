// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -enable-copy-propagation=false -Xllvm -sil-print-all -module-name=main %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -enable-lexical-lifetimes=false -Xllvm -sil-print-all -module-name=main %s 2>&1 | %FileCheck %s

@inline(never)
func takeGuaranteed(_ a: AnyObject) -> AnyObject {
  return a
}

// CHECK-LABEL: // testLexical(a:)
// CHECK: [[B:%.*]] = begin_borrow [lexical] %0
// CHECK: apply %{{.*}}([[B]])
// CHECK: apply
// CHECK: end_borrow [[B]]
// CHECK-LABEL: } // end sil function

// LexicalLifetimeEliminator must strip the [lexical] flag
// before the first round of SemanticARCOpts.

// CHECK-NOT: *** SIL function after {{.*}} (semantic-arc-opts)

// CHECK-LABEL: *** SIL function after {{.*}} (sil-lexical-lifetime-eliminator)
// CHECK-LABEL: // testLexical(a:)
// CHECK: [[B:%.*]] = begin_borrow %0
// CHECK: apply %{{.*}}([[B]])
// CHECK: apply
// CHECK: end_borrow [[B]]
// CHECK-LABEL: } // end sil function

// The first round of SemanticARCOpts must eliminate the borrow scope
// that was only needed for a lexical lifetime.

// CHECK-LABEL: *** SIL function after {{.*}} (semantic-arc-opts)
// CHECK-LABEL: // testLexical(a:)
// CHECK: apply %{{.*}}(%0)
// CHECK-LABEL: } // end sil function
public func testLexical(a: __owned AnyObject) -> AnyObject {
  // Without lexical lifetimes, the lifetime of 'a' ends in between the two calls:
  return takeGuaranteed(takeGuaranteed(a))
}
