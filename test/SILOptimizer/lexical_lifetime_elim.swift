// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -enable-copy-propagation=false -Xllvm -sil-print-all -module-name=main %s 2>&1 | %FileCheck %s --check-prefixes CHECK,CHECK-NOCOPYPROP
// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -enable-lexical-lifetimes=false -Xllvm -sil-print-all -module-name=main %s 2>&1 | %FileCheck %s --check-prefixes CHECK,CHECK-COPYPROP

// REQUIRES: swift_in_compiler

@inline(never)
func takeGuaranteed(_ a: AnyObject) -> AnyObject {
  return a
}

@_silgen_name("getOwned")
@inline(never)
func getOwned() -> AnyObject

// CHECK-LABEL: // testLexical()
// CHECK: [[A:%.*]] = apply %{{.*}}()
// CHECK: [[B:%.*]] = move_value [lexical] [var_decl] [[A]]
// CHECK: [[BB:%.*]] = begin_borrow [[B]]
// CHECK: apply %{{.*}}([[BB]])
// CHECK: end_borrow [[BB]]
// CHECK: apply
// CHECK: destroy_value [[B]]
// CHECK-LABEL: } // end sil function

// LexicalLifetimeEliminator must strip the [lexical] flag
// before the first round of SemanticARCOpts.

// CHECK-NOT: *** SIL function after {{.*}} (semantic-arc-opts)

// CHECK-LABEL: *** SIL function after {{.*}} (sil-lexical-lifetime-eliminator)
// CHECK-LABEL: // testLexical()
// CHECK: [[A:%.*]] = apply %{{.*}}()
// CHECK: [[B:%.*]] = move_value [var_decl] [[A]]
// CHECK: apply %{{.*}}([[B]])
// CHECK: apply
// CHECK: destroy_value [[B]]
// CHECK-LABEL: } // end sil function

// The first round of SemanticARCOpts/CopyPropagation must eliminate the
// redundant move_value that was only needed for a lexical lifetime.

// CHECK-NOCOPYPROP-LABEL: *** SIL function after {{.*}} (semantic-arc-opts)
// CHECK-COPYPROP-LABEL: *** SIL function after {{.*}} (copy-propagation)
// CHECK-LABEL: // testLexical()
// CHECK: [[A:%.*]] = apply %{{.*}}()
// CHECK: apply %{{.*}}([[A]])
// CHECK-LABEL: } // end sil function
public func testLexical() -> AnyObject {
  let a = getOwned()
  // Without lexical lifetimes, the lifetime of 'a' ends in between the two calls:
  return takeGuaranteed(takeGuaranteed(a))
}
