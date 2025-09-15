// RUN: %target-swift-frontend -target x86_64-apple-macosx99.99 -parse-as-library -O -module-name=test %s -Xllvm -sil-print-types -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx12.3 -parse-as-library -O -module-name=test %s -Xllvm -sil-print-types -emit-sil | %FileCheck %s --check-prefix=NOOPTCLASS

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: OS=macosx

// CHECK: sil @$s4test12makeArrayInt1xSaySiGSi_tF
// CHECK:   alloc_ref [tail_elems $Int * {{.*}} : $Builtin.Word] $_ContiguousArrayStorage<Int>
// NOOPTCLASS: sil @$s4test12makeArrayInt1xSaySiGSi_tF
// NOOPTCLASS:   alloc_ref [tail_elems $Int * {{.*}} : $Builtin.Word] $_ContiguousArrayStorage<Int>
public func makeArrayInt(x: Int) -> [Int] {
  return [0, 1, x]
}

// CHECK: sil {{.*}}@$s4test14makeArrayClass1xSayAA1CCGAE_tF
// CHECK:   alloc_ref [tail_elems $C * {{.*}} : $Builtin.Word] $_ContiguousArrayStorage<AnyObject>
// NOOPTCLASS: sil {{.*}}@$s4test14makeArrayClass1xSayAA1CCGAE_tF
// NOOPTCLASS:   function_ref @$ss29getContiguousArrayStorageType3fors01
// NOOPTCLASS:   alloc_ref_dynamic [tail_elems $C * {{.*}} : $Builtin.Word]
public class C {}
public func makeArrayClass(x: C) -> [C] {
  return [x, x, x]
}
