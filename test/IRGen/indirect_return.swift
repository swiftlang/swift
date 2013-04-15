// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -sil-irgen | FileCheck %s

// CHECK: define void @_T15indirect_return11generic_getU__FT1pGVSs13UnsafePointerQ___Q_
func generic_get<T>(p:UnsafePointer<T>) -> T {
  // CHECK: call void @_TVSs13UnsafePointer3getU__fRGS_Q__FT_Q_(%swift.opaque* noalias sret {{%.*}}, %VSs13UnsafePointer* {{%.*}}, %swift.type* {{%.*}})
  return p.get()
}
