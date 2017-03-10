// RUN: %target-swift-frontend -primary-file %s -emit-ir > %t
// RUN: FileCheck %s --check-prefix=CHECK-ONONE-NODEBUG --check-prefix=CHECK < %t
// RUN: %target-swift-frontend -primary-file %s -emit-ir -g > %t.Onone-debug
// RUN: FileCheck %s --check-prefix=CHECK-ONONE-DEBUG --check-prefix=CHECK < %t.Onone-debug

// Check that unused user-defined functions are preserved at -Onone when -g is used.

// Check that both user functions are marked as used.when using -Onone -g
// CHECK-ONONE-DEBUG: llvm.used{{.*}}_TF6unused7publicFFT_T_{{.*}}_TF6unusedP33_E4A5FE4B57779A0FC903FD67CDBCAAFB8privateFFT_T_{{.*}}

// Check that private function is not marked as used when -g is not used.
// CHECK-ONONE-NODEBUG-NOT: llvm.used{{.*}}_TF6unusedP33_E4A5FE4B57779A0FC903FD67CDBCAAFB8privateFFT_T_{{.*}}

// CHECK-ONONE-DEBUG-LABEL: define{{.*}}void @_TF6unusedP33_E4A5FE4B57779A0FC903FD67CDBCAAFB8privateFFT_T_
// CHECK-ONONE-NODEBUG-NOT: @_TF6unusedP33_E4A5FE4B57779A0FC903FD67CDBCAAFB8privateFFT_T_
private func privateF() {
  print(1)
}

// CHECK-LABEL: define{{.*}}void @_TF6unused7publicFFT_T_() 
public func publicF() {
  print(2)
}

