// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

func inoutWithDefaults(_ x: inout Int, y: Int = 0, z: Int = 0) {}
func inoutWithCallerSideDefaults(_ x: inout Int, y: Int = #line) {}

func scalarWithDefaults(_ x: Int, y: Int = 0, z: Int = 0) {}
func scalarWithCallerSideDefaults(_ x: Int, y: Int = #line) {}

func tupleWithDefaults(x x: (Int, Int), y: Int = 0, z: Int = 0) {}

func variadicFirst(_ x: Int...) {}
func variadicSecond(_ x: Int, _ y: Int...) {}

var x = 0
// CHECK: [[X_ADDR:%.*]] = global_addr @_T020scalar_to_tuple_args1xSivp : $*Int
// CHECK: [[DEFAULT_Y:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[DEFAULT_Z:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[INOUT_WITH_DEFAULTS:%.*]] = function_ref @_T020scalar_to_tuple_args17inoutWithDefaultsySiz_Si1ySi1ztF
// CHECK: apply [[INOUT_WITH_DEFAULTS]]([[WRITE]], [[DEFAULT_Y]], [[DEFAULT_Z]])
inoutWithDefaults(&x)

// CHECK: [[LINE_VAL:%.*]] = integer_literal
// CHECK: [[LINE:%.*]] = apply {{.*}}([[LINE_VAL]]
// CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[INOUT_WITH_CALLER_DEFAULTS:%.*]] = function_ref @_T020scalar_to_tuple_args27inoutWithCallerSideDefaultsySiz_Si1ytF
// CHECK: apply [[INOUT_WITH_CALLER_DEFAULTS]]([[WRITE]], [[LINE]])
inoutWithCallerSideDefaults(&x)

// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X:%.*]] = load [trivial] [[READ]]
// CHECK: [[DEFAULT_Y:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[DEFAULT_Z:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[SCALAR_WITH_DEFAULTS:%.*]] = function_ref @_T020scalar_to_tuple_args0A12WithDefaultsySi_Si1ySi1ztF
// CHECK: apply [[SCALAR_WITH_DEFAULTS]]([[X]], [[DEFAULT_Y]], [[DEFAULT_Z]])
scalarWithDefaults(x)

// CHECK: [[X:%.*]] = load [trivial] [[X_ADDR]]
// CHECK: [[LINE_VAL:%.*]] = integer_literal
// CHECK: [[LINE:%.*]] = apply {{.*}}([[LINE_VAL]]
// CHECK: [[SCALAR_WITH_CALLER_DEFAULTS:%.*]] = function_ref @_T020scalar_to_tuple_args0A22WithCallerSideDefaultsySi_Si1ytF
// CHECK: apply [[SCALAR_WITH_CALLER_DEFAULTS]]([[X]], [[LINE]])
scalarWithCallerSideDefaults(x)

// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X1:%.*]] = load [trivial] [[READ]]
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X2:%.*]] = load [trivial] [[READ]]
// CHECK: [[DEFAULT_Y:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[DEFAULT_Z:%.*]] = apply {{.*}} : $@convention(thin) () -> Int
// CHECK: [[TUPLE_WITH_DEFAULTS:%.*]] = function_ref @_T020scalar_to_tuple_args0C12WithDefaultsySi_Sit1x_Si1ySi1ztF
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
// CHECK: [[VARIADIC_FIRST:%.*]] = function_ref @_T020scalar_to_tuple_args13variadicFirstySid_tF
// CHECK: apply [[VARIADIC_FIRST]]([[ARRAY]])
variadicFirst(x)

// CHECK: [[ALLOC_ARRAY:%.*]] = apply {{.*}} -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [[BORROWED_ALLOC_ARRAY:%.*]] = begin_borrow [[ALLOC_ARRAY]]
// CHECK: [[BORROWED_ARRAY:%.*]] = tuple_extract [[BORROWED_ALLOC_ARRAY]] {{.*}}, 0
// CHECK: [[ARRAY:%.*]] = copy_value [[BORROWED_ARRAY]]
// CHECK: end_borrow [[BORROWED_ALLOC_ARRAY]] from [[ALLOC_ARRAY]]
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[X_ADDR]] : $*Int
// CHECK: [[X:%.*]] = load [trivial] [[READ]]
// CHECK: [[VARIADIC_SECOND:%.*]] = function_ref @_T020scalar_to_tuple_args14variadicSecondySi_SidtF
// CHECK: apply [[VARIADIC_SECOND]]([[X]], [[ARRAY]])
variadicSecond(x)
