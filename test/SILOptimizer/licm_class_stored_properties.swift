// RUN: %target-swift-frontend -O -module-name test -emit-sil -primary-file %s | %FileCheck %s
// RUN: %target-swift-frontend -O -module-name test -emit-ir -primary-file %s | %FileCheck %s --check-prefix=CHECK-IR

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib
// REQUIRES: CPU=arm64 || CPU=x86_64

// Check that the loads of the buffer pointers are hoisted out of the loop, so
// that the loop can be vectorized.
// This requires to rotate the loop after LICM has hoisted the access to
// `dst.count` out of the loop header. And LICM must hoist the read accesses
// to `dst.ptr` and `src.ptr`, even if they may access the same memory.

public final class ByteVec {
  var ptr: UnsafeMutablePointer<UInt8>
  var count: Int

  init(_ n: Int) {
    ptr = .allocate(capacity: n)
    count = n
  }
}

// CHECK-LABEL: sil @$s4test6addOneyyAA7ByteVecC_ADtF :
// CHECK:         struct_element_addr {{%[0-9]+}}, #UnsafeMutablePointer._rawValue
// CHECK:         struct_element_addr {{%[0-9]+}}, #UnsafeMutablePointer._rawValue
// CHECK:       // Loop header
// CHECK-NEXT:  bb{{[0-9]+}}(
// CHECK-NOT:     #UnsafeMutablePointer._rawValue
// CHECK-NOT:     begin_access
// CHECK:         store
// CHECK-NOT:     #UnsafeMutablePointer._rawValue
// CHECK-NOT:     begin_access
// CHECK:         cond_br
// CHECK:       } // end sil function '$s4test6addOneyyAA7ByteVecC_ADtF'

// CHECK-IR-LABEL: define {{.*}} @"$s4test6addOneyyAA7ByteVecC_ADtF"
// CHECK-IR: vector.body:
// CHECK-IR:   load <{{[0-9]+}} x i8>
// CHECK-IR:   add <{{[0-9]+}} x i8>
// CHECK-IR:   store <{{[0-9]+}} x i8>
public func addOne(_ dst: ByteVec, _ src: ByteVec) {
  var i = 0
  while i < dst.count {
    dst.ptr[i] = src.ptr[i] &+ 1
    i &+= 1
  }
}
