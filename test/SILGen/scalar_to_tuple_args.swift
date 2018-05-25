
// RUN: %target-swift-emit-silgen -module-name scalar_to_tuple_args -enable-sil-ownership %s | %FileCheck %s

func inoutWithDefaults(_ x: inout Int, y: Int = 0, z: Int = 0) {}
func inoutWithCallerSideDefaults(_ x: inout Int, y: Int = #line) {}

func scalarWithDefaults(_ x: Int, y: Int = 0, z: Int = 0) {}
func scalarWithCallerSideDefaults(_ x: Int, y: Int = #line) {}

func tupleWithDefaults(x x: (Int, Int), y: Int = 0, z: Int = 0) {}

func variadicFirst(_ x: Int...) {}
func variadicSecond(_ x: Int, _ y: Int...) {}

var x = 0
// CHECK: [[X_ADDR:%.*]] = global_addr @$S20scalar_to_tuple_args1xSivp : $*Int
// CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[DEFAULT_Y:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[DEFAULT_Z:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[INOUT_WITH_DEFAULTS:%.*]] = function_ref @$S20scalar_to_tuple_args17inoutWithDefaults_1y1zySiz_S2itF
// CHECK: apply [[INOUT_WITH_DEFAULTS]]([[WRITE]], [[DEFAULT_Y]], [[DEFAULT_Z]])
inoutWithDefaults(&x)

// CHECK: [[LINE_VAL:%.*]] = integer_literal
// CHECK: [[LINE:%.*]] = apply {{.*}}([[LINE_VAL]]
// CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[INOUT_WITH_CALLER_DEFAULTS:%.*]] = function_ref @$S20scalar_to_tuple_args27inoutWithCallerSideDefaults_1yySiz_SitF
// CHECK: apply [[INOUT_WITH_CALLER_DEFAULTS]]([[WRITE]], [[LINE]])
inoutWithCallerSideDefaults(&x)

// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X:%.*]] = load [trivial] [[READ]]
// CHECK: [[DEFAULT_Y:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[DEFAULT_Z:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[SCALAR_WITH_DEFAULTS:%.*]] = function_ref @$S20scalar_to_tuple_args0A12WithDefaults_1y1zySi_S2itF
// CHECK: apply [[SCALAR_WITH_DEFAULTS]]([[X]], [[DEFAULT_Y]], [[DEFAULT_Z]])
scalarWithDefaults(x)

// CHECK: [[X:%.*]] = load [trivial] [[X_ADDR]]
// CHECK: [[LINE_VAL:%.*]] = integer_literal
// CHECK: [[LINE:%.*]] = apply {{.*}}([[LINE_VAL]]
// CHECK: [[SCALAR_WITH_CALLER_DEFAULTS:%.*]] = function_ref @$S20scalar_to_tuple_args0A22WithCallerSideDefaults_1yySi_SitF
// CHECK: apply [[SCALAR_WITH_CALLER_DEFAULTS]]([[X]], [[LINE]])
scalarWithCallerSideDefaults(x)

// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X1:%.*]] = load [trivial] [[READ]]
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X2:%.*]] = load [trivial] [[READ]]
// CHECK: [[DEFAULT_Y:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[DEFAULT_Z:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[TUPLE_WITH_DEFAULTS:%.*]] = function_ref @$S20scalar_to_tuple_args0C12WithDefaults1x1y1zySi_Sit_S2itF
// CHECK: apply [[TUPLE_WITH_DEFAULTS]]([[X1]], [[X2]], [[DEFAULT_Y]], [[DEFAULT_Z]])
tupleWithDefaults(x: (x,x))

// CHECK: [[ALLOC_ARRAY:%.*]] = apply {{.*}} -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [[BORROWED_ALLOC_ARRAY:%.*]] = begin_borrow [[ALLOC_ARRAY]]
// CHECK: [[BORROWED_ARRAY:%.*]] = tuple_extract [[BORROWED_ALLOC_ARRAY]] {{.*}}, 0
// CHECK: [[ARRAY:%.*]] = copy_value [[BORROWED_ARRAY]]
// CHECK: [[MEMORY:%.*]] = tuple_extract [[BORROWED_ALLOC_ARRAY]] {{.*}}, 1
// CHECK: end_borrow [[BORROWED_ALLOC_ARRAY]] from [[ALLOC_ARRAY]]
// CHECK: destroy_value [[ALLOC_ARRAY]]
// CHECK: [[ADDR:%.*]] = pointer_to_address [[MEMORY]]
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X:%.*]] = load [trivial] [[READ]]
// CHECK: store [[X]] to [trivial] [[ADDR]]
// CHECK: [[BORROWED_ARRAY:%.*]] = begin_borrow [[ARRAY]]
// CHECK: [[VARIADIC_FIRST:%.*]] = function_ref @$S20scalar_to_tuple_args13variadicFirstyySid_tF
// CHECK: apply [[VARIADIC_FIRST]]([[BORROWED_ARRAY]])
variadicFirst(x)

// CHECK: [[ALLOC_ARRAY:%.*]] = apply {{.*}} -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [[BORROWED_ALLOC_ARRAY:%.*]] = begin_borrow [[ALLOC_ARRAY]]
// CHECK: [[BORROWED_ARRAY:%.*]] = tuple_extract [[BORROWED_ALLOC_ARRAY]] {{.*}}, 0
// CHECK: [[ARRAY:%.*]] = copy_value [[BORROWED_ARRAY]]
// CHECK: end_borrow [[BORROWED_ALLOC_ARRAY]] from [[ALLOC_ARRAY]]
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X:%.*]] = load [trivial] [[READ]]
// CHECK: [[BORROWED_ARRAY:%.*]] = begin_borrow [[ARRAY]]
// CHECK: [[VARIADIC_SECOND:%.*]] = function_ref @$S20scalar_to_tuple_args14variadicSecondyySi_SidtF
// CHECK: apply [[VARIADIC_SECOND]]([[X]], [[BORROWED_ARRAY]])
variadicSecond(x)
