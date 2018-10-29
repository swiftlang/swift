// RUN: %target-swift-frontend -parse-as-library -O %s -emit-ir > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck -check-prefix=CHECK-REFLECTION %s < %t.ll

// No global metadata, witness tables, etc. Only reflection metadata should not be optimized away
// CHECK-NOT: @"$s{{.*[^F] =}}"
// CHECK-REFLECTION: @[[C:.*]] = linkonce_odr hidden constant {{.*}} @"$s10unusedtype13MicroSequenceVMn"
// CHECK-REFLECTION: @"$s10unusedtype13MicroSequenceVMF" = {{.*}} @[[C]]

// No conformance records
// CHECK-NOT: protocol_conformances

// No functions
// CHECK-NOT: define

struct MicroSequence : Sequence, IteratorProtocol {
  func next() -> Int? {
    return nil
  }
}

