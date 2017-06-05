// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s

// CHECK: sil_global hidden_external [[DSO:@__dso_handle]] : $Builtin.RawPointer

// CHECK-LABEL: sil @main : $@convention(c)
// CHECK: bb0
// CHECK: [[DSOAddr:%[0-9]+]] = global_addr [[DSO]] : $*Builtin.RawPointer
// CHECK-NEXT: [[DSOPtr:%[0-9]+]] = address_to_pointer [[DSOAddr]] : $*Builtin.RawPointer to $Builtin.RawPointer
// CHECK-NEXT: [[DSOPtrStruct:[0-9]+]] = struct $UnsafeRawPointer ([[DSOPtr]] : $Builtin.RawPointer)


// CHECK-LABEL: sil hidden @_T010dso_handle14printDSOHandleS2V0A0_tFfA_
// CHECK: [[DSOAddr:%[0-9]+]] = global_addr [[DSO]] : $*Builtin.RawPointer
// CHECK-NEXT: [[DSOPtr:%[0-9]+]] = address_to_pointer [[DSOAddr]] : $*Builtin.RawPointer to $Builtin.RawPointer
// CHECK-NEXT: [[DSOPtrStruct:%[0-9]+]] = struct $UnsafeRawPointer ([[DSOPtr]] : $Builtin.RawPointer)
// CHECK-NEXT: return [[DSOPtrStruct]] : $UnsafeRawPointer
func printDSOHandle(dso: UnsafeRawPointer = #dsohandle) -> UnsafeRawPointer {
  print(dso)
  return dso
}

_ = printDSOHandle()

