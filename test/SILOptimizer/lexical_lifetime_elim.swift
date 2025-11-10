// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -enable-copy-propagation=false -module-name=main %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -enable-lexical-lifetimes=false -module-name=main %s | %FileCheck %s

// REQUIRES: swift_in_compiler

@_silgen_name("takeGuaranteed")
@inline(never)
func takeGuaranteed(_ a: AnyObject) -> AnyObject

@_silgen_name("getOwned")
@inline(never)
func getOwned() -> AnyObject

// CHECK-LABEL: sil @$s4main11testLexicalyXlyF :
// CHECK:         [[A:%.*]] = apply
// CHECK:         [[B:%.*]] = apply
// CHECK-DAG:     strong_release [[B]]
// CHECK-DAG:     strong_release [[A]]
// CHECK-LABEL: } // end sil function '$s4main11testLexicalyXlyF'
public func testLexical() -> AnyObject {
  let a = getOwned()
  // Without lexical lifetimes, the lifetime of 'a' ends in between the two calls:
  return takeGuaranteed(takeGuaranteed(a))
}
