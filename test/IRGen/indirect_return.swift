// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

// CHECK: define void @_TF15indirect_return11generic_get
func generic_get<T>(p: UnsafePointer<T>) -> T {
  // CHECK: call void @_TFVSs13UnsafePointer3get{{.*}}(%swift.opaque* noalias sret {{%.*}}, i8* {{.*}}, %swift.type* {{%.*}})
  return p.get()
}
