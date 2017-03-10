// RUN: %target-swift-frontend -parse-as-library -Xllvm -new-mangling-for-tests -O %s -emit-ir > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck -check-prefix=CHECK-REFLECTION %s < %t.ll

// No global metadata, witness tables, etc. Only reflection metadata should not be optimized away
// CHECK-NOT: @_T0{{.*[^F] =}}
// CHECK-REFLECTION: @[[C:[0-9]+]] = private constant {{.*}} c"10unusedtype13MicroSequenceV\00"
// CHECK-REFLECTION: @_T010unusedtype13MicroSequenceVMF = {{.*}} [[C]]

// No conformance records
// CHECK-NOT: protocol_conformances

// No functions
// CHECK-NOT: define

struct MicroSequence : Sequence, IteratorProtocol {
  func next() -> Int? {
    return nil
  }
}

