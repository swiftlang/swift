// RUN: %target-swift-frontend %s -profile-generate -emit-sil -o %t.sil
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %t.sil -module-name=coverage_ignored -emit-ir -o - | %FileCheck %s -check-prefix=CHECK-IGNORED

// CHECK-IGNORED-NOT: llvm.instrprof
// CHECK-IGNORED-NOT: profc
func foo() {}
foo()
