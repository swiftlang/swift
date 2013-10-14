
//// Automatically Generated From AxleBuiltins.gyb.  Do Not Edit Directly ////
//// To regenerate:                                                       ////
////         ../../tools/gyb AxleBuiltins.gyb -o AxleBuiltins.swift       ////

// 2.2.1 Vertex Functions
@asmname="llvm.air.get_vertex_id"
func getVertexId() -> Int32

@asmname="llvm.air.get_instance_id"
func getInstanceId() -> Int32

// 2.3.1 Work-Item Functions
@asmname="llvm.air.get_work_dim.i32"
func getWorkDim() -> Int32

@asmname="llvm.air.get_global_id.i32"
func getGlobalId(dimindx : Int32) -> Int32

@asmname="llvm.air.get_global_size.i32"
func getGlobalSize(dimindx : Int32) -> Int32

