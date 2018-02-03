// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -profile-generate -emit-sil -o %t.sil
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir -o - | %FileCheck %s

// CHECK-NOT: llvm.instrprof
// CHECK-NOT: profc
func foo() {}
foo()
