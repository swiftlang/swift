// RUN: %swift -std=axle -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

@kernel func ktest() {
  // Compute functions
  getWorkDim()
  getGlobalId(0)
  getGlobalSize(1)
}

// CHECK-LABEL: call i32 @llvm.air.get_work_dim.i32()
// CHECK-LABEL: call i32 @llvm.air.get_global_id.i32(i32 0)
// CHECK-LABEL: call i32 @llvm.air.get_global_size.i32(i32 1)

@vertex func vtest() {
  // Vertex functions
  getVertexId()
  getInstanceId()
}

// CHECK-LABEL: call i32 @llvm.air.get_vertex_id()
// CHECK-LABEL: call i32 @llvm.air.get_instance_id()
