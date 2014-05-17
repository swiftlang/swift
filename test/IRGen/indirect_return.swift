// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

// CHECK: define void @_TF15indirect_return11generic_get
func generic_get<T>(p: UnsafePointer<T>) -> T {
  // CHECK: call void @_TFVSs13UnsafePointerg6memoryQ_(%swift.opaque* noalias sret %0, i8* %1, %swift.type* %T)
  return p.memory
}
