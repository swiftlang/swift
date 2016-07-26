// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen %s | FileCheck %s

// CHECK: sil_global hidden_external @__dso_handle : $UnsafeMutablePointer<()>

// CHECK-LABEL: sil @main : $@convention(c)
// CHECK: bb0
// CHECK: [[DSO:%[0-9]+]] = global_addr @__dso_handle : $*UnsafeMutablePointer<()>
// CHECK: load [[DSO]]

// CHECK-LABEL: sil hidden @_TIF10dso_handle14printDSOHandleFT3dsoGSpT___GSpT__A_
// CHECK: [[DSO:%[0-9]+]] = global_addr @__dso_handle : $*UnsafeMutablePointer<()>
// CHECK: load [[DSO]]
func printDSOHandle(dso: UnsafeMutablePointer<Void> = #dsohandle) -> UnsafeMutablePointer<Void> {
  print(dso)
}

printDSOHandle()

