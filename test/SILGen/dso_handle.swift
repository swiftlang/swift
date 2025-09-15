// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -Xllvm -sil-full-demangle %s | %FileCheck %s

// CHECK: sil_global {{.*}}[[DSO:@__dso_handle|@__ImageBase]] : $Builtin.RawPointer

// CHECK-LABEL: sil [ossa] @main : $@convention(c)
// CHECK: bb0
// CHECK: [[DSOAddr:%[0-9]+]] = global_addr [[DSO]] : $*Builtin.RawPointer
// CHECK-NEXT: [[DSOPtr:%[0-9]+]] = address_to_pointer [[DSOAddr]] : $*Builtin.RawPointer to $Builtin.RawPointer
// CHECK-NEXT: [[DSOPtrStruct:[0-9]+]] = struct $UnsafeRawPointer ([[DSOPtr]] : $Builtin.RawPointer)

func printDSOHandle(dso: UnsafeRawPointer = #dsohandle) -> UnsafeRawPointer {
  print(dso)
  return dso
}

@inlinable public func printDSOHandleInlinable(dso: UnsafeRawPointer = #dsohandle) -> UnsafeRawPointer {
  return dso
}

@inlinable public func callsPrintDSOHandleInlinable() {
  printDSOHandleInlinable()
}

_ = printDSOHandle()
