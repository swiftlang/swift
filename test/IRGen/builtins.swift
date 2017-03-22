// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -parse-stdlib -primary-file %s -emit-ir -o - -disable-objc-attr-requires-foundation-module | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime
// REQUIRES: executable_test

// REQUIRES: CPU=x86_64

import Swift

// CHECK-DAG: [[REFCOUNT:%swift.refcounted.*]] = type
// CHECK-DAG: [[X:%T8builtins1XC]] = type
// CHECK-DAG: [[Y:%T8builtins1YC]] = type

typealias Int = Builtin.Int32
typealias Bool = Builtin.Int1

infix operator * {
  associativity left
  precedence 200
}
infix operator / {
  associativity left
  precedence 200
}
infix operator % {
  associativity left
  precedence 200
}

infix operator + {
  associativity left
  precedence 190
}
infix operator - {
  associativity left
  precedence 190
}

infix operator << {
  associativity none
  precedence 180
}
infix operator >> {
  associativity none
  precedence 180
}

infix operator ... {
  associativity none
  precedence 175
}

infix operator < {
  associativity none
  precedence 170
}
infix operator <= {
  associativity none
  precedence 170
}
infix operator > {
  associativity none
  precedence 170
}
infix operator >= {
  associativity none
  precedence 170
}

infix operator == {
  associativity none
  precedence 160
}
infix operator != {
  associativity none
  precedence 160
}

func * (lhs: Int, rhs: Int) -> Int {
  return Builtin.mul_Int32(lhs, rhs)
  // CHECK: mul i32
}
func / (lhs: Int, rhs: Int) -> Int {
  return Builtin.sdiv_Int32(lhs, rhs)
  // CHECK: sdiv i32
}
func % (lhs: Int, rhs: Int) -> Int {
  return Builtin.srem_Int32(lhs, rhs)
  // CHECK: srem i32
}
func + (lhs: Int, rhs: Int) -> Int {
  return Builtin.add_Int32(lhs, rhs)
  // CHECK: add i32
}
func - (lhs: Int, rhs: Int) -> Int {
  return Builtin.sub_Int32(lhs, rhs)
  // CHECK: sub i32
}
// In C, 180 is <<, >>
func < (lhs: Int, rhs: Int) -> Bool {
  return Builtin.cmp_slt_Int32(lhs, rhs)
  // CHECK: icmp slt i32
}
func > (lhs: Int, rhs: Int) -> Bool {
  return Builtin.cmp_sgt_Int32(lhs, rhs)
  // CHECK: icmp sgt i32
}
func <=(lhs: Int, rhs: Int) -> Bool {
  return Builtin.cmp_sle_Int32(lhs, rhs)
  // CHECK: icmp sle i32
}
func >=(lhs: Int, rhs: Int) -> Bool {
  return Builtin.cmp_sge_Int32(lhs, rhs)
  // CHECK: icmp sge i32
}
func ==(lhs: Int, rhs: Int) -> Bool {
  return Builtin.cmp_eq_Int32(lhs, rhs)
  // CHECK: icmp eq i32
}
func !=(lhs: Int, rhs: Int) -> Bool {
  return Builtin.cmp_ne_Int32(lhs, rhs)
  // CHECK: icmp ne i32
}

func gepRaw_test(_ ptr: Builtin.RawPointer, offset: Builtin.Int64)
   -> Builtin.RawPointer {
  return Builtin.gepRaw_Int64(ptr, offset)
  // CHECK: getelementptr inbounds i8, i8*
}

// CHECK: define hidden {{.*}}i64 @_T08builtins9load_test{{[_0-9a-zA-Z]*}}F
func load_test(_ ptr: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[CASTPTR:%.*]] = bitcast i8* [[PTR:%.*]] to i64*
  // CHECK-NEXT: load i64, i64* [[CASTPTR]]
  // CHECK: ret
  return Builtin.load(ptr)
}

// CHECK: define hidden {{.*}}i64 @_T08builtins13load_raw_test{{[_0-9a-zA-Z]*}}F
func load_raw_test(_ ptr: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[CASTPTR:%.*]] = bitcast i8* [[PTR:%.*]] to i64*
  // CHECK-NEXT: load i64, i64* [[CASTPTR]]
  // CHECK: ret
  return Builtin.loadRaw(ptr)
}

// CHECK: define hidden {{.*}}void @_T08builtins11assign_test{{[_0-9a-zA-Z]*}}F
func assign_test(_ value: Builtin.Int64, ptr: Builtin.RawPointer) {
  Builtin.assign(value, ptr)
  // CHECK: ret
}

// CHECK: define hidden {{.*}}%swift.refcounted* @_T08builtins16load_object_test{{[_0-9a-zA-Z]*}}F
func load_object_test(_ ptr: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[T0:%.*]] = load [[REFCOUNT]]*, [[REFCOUNT]]**
  // CHECK: call void @swift_rt_swift_retain([[REFCOUNT]]* [[T0]])
  // CHECK: ret [[REFCOUNT]]* [[T0]]
  return Builtin.load(ptr)
}

// CHECK: define hidden {{.*}}%swift.refcounted* @_T08builtins20load_raw_object_test{{[_0-9a-zA-Z]*}}F
func load_raw_object_test(_ ptr: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[T0:%.*]] = load [[REFCOUNT]]*, [[REFCOUNT]]**
  // CHECK: call void @swift_rt_swift_retain([[REFCOUNT]]* [[T0]])
  // CHECK: ret [[REFCOUNT]]* [[T0]]
  return Builtin.loadRaw(ptr)
}

// CHECK: define hidden {{.*}}void @_T08builtins18assign_object_test{{[_0-9a-zA-Z]*}}F
func assign_object_test(_ value: Builtin.NativeObject, ptr: Builtin.RawPointer) {
  Builtin.assign(value, ptr)
}

// CHECK: define hidden {{.*}}void @_T08builtins16init_object_test{{[_0-9a-zA-Z]*}}F
func init_object_test(_ value: Builtin.NativeObject, ptr: Builtin.RawPointer) {
  // CHECK: [[DEST:%.*]] = bitcast i8* {{%.*}} to %swift.refcounted**
  // CHECK-NEXT: store [[REFCOUNT]]* {{%.*}}, [[REFCOUNT]]** [[DEST]]
  Builtin.initialize(value, ptr)
}

func cast_test(_ ptr: inout Builtin.RawPointer, i8: inout Builtin.Int8,
               i64: inout Builtin.Int64, f: inout Builtin.FPIEEE32,
               d: inout Builtin.FPIEEE64
) {
  // CHECK: cast_test

  i8 = Builtin.trunc_Int64_Int8(i64)    // CHECK: trunc
  i64 = Builtin.zext_Int8_Int64(i8)     // CHECK: zext
  i64 = Builtin.sext_Int8_Int64(i8)     // CHECK: sext
  i64 = Builtin.ptrtoint_Int64(ptr)     // CHECK: ptrtoint
  ptr = Builtin.inttoptr_Int64(i64)     // CHECK: inttoptr
  i64 = Builtin.fptoui_FPIEEE64_Int64(d) // CHECK: fptoui
  i64 = Builtin.fptosi_FPIEEE64_Int64(d) // CHECK: fptosi
  d = Builtin.uitofp_Int64_FPIEEE64(i64) // CHECK: uitofp
  d = Builtin.sitofp_Int64_FPIEEE64(i64) // CHECK: sitofp
  d = Builtin.fpext_FPIEEE32_FPIEEE64(f) // CHECK: fpext
  f = Builtin.fptrunc_FPIEEE64_FPIEEE32(d) // CHECK: fptrunc
  i64 = Builtin.bitcast_FPIEEE64_Int64(d)   // CHECK: bitcast
  d = Builtin.bitcast_Int64_FPIEEE64(i64)   // CHECK: bitcast
}

func intrinsic_test(_ i32: inout Builtin.Int32, i16: inout Builtin.Int16) {
  i32 = Builtin.int_bswap_Int32(i32) // CHECK: llvm.bswap.i32(

  i16 = Builtin.int_bswap_Int16(i16) // CHECK: llvm.bswap.i16(
  
  var x = Builtin.int_sadd_with_overflow_Int16(i16, i16) // CHECK: call { i16, i1 } @llvm.sadd.with.overflow.i16(
  
  Builtin.int_trap() // CHECK: llvm.trap()
}

// CHECK: define hidden {{.*}}void @_T08builtins19sizeof_alignof_testyyF()
func sizeof_alignof_test() {
  // CHECK: store i64 4, i64*
  var xs = Builtin.sizeof(Int.self) 
  // CHECK: store i64 4, i64*
  var xa = Builtin.alignof(Int.self) 
  // CHECK: store i64 1, i64*
  var ys = Builtin.sizeof(Bool.self) 
  // CHECK: store i64 1, i64*
  var ya = Builtin.alignof(Bool.self) 

}

// CHECK: define hidden {{.*}}void @_T08builtins27generic_sizeof_alignof_testyxlF(
func generic_sizeof_alignof_test<T>(_: T) {
  // CHECK:      [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T:%.*]], i32 17
  // CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]]
  // CHECK-NEXT: [[SIZE:%.*]] = ptrtoint i8* [[T1]] to i64
  // CHECK-NEXT: store i64 [[SIZE]], i64* [[S:%.*]]
  var s = Builtin.sizeof(T.self)
  // CHECK: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T:%.*]], i32 18
  // CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]]
  // CHECK-NEXT: [[T2:%.*]] = ptrtoint i8* [[T1]] to i64
  // CHECK-NEXT: [[T3:%.*]] = and i64 [[T2]], 65535
  // CHECK-NEXT: [[ALIGN:%.*]] = add i64 [[T3]], 1
  // CHECK-NEXT: store i64 [[ALIGN]], i64* [[A:%.*]]
  var a = Builtin.alignof(T.self)
}

// CHECK: define hidden {{.*}}void @_T08builtins21generic_strideof_testyxlF(
func generic_strideof_test<T>(_: T) {
  // CHECK:      [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T:%.*]], i32 19
  // CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]]
  // CHECK-NEXT: [[STRIDE:%.*]] = ptrtoint i8* [[T1]] to i64
  // CHECK-NEXT: store i64 [[STRIDE]], i64* [[S:%.*]]
  var s = Builtin.strideof(T.self)
}

class X {}

class Y {}
func move(_ ptr: Builtin.RawPointer) {
  var temp : Y = Builtin.take(ptr)
  // CHECK:      define hidden {{.*}}void @_T08builtins4move{{[_0-9a-zA-Z]*}}F
  // CHECK:        [[SRC:%.*]] = bitcast i8* {{%.*}} to [[Y]]**
  // CHECK-NEXT:   [[VAL:%.*]] = load [[Y]]*, [[Y]]** [[SRC]]
  // CHECK-NEXT:   store [[Y]]* [[VAL]], [[Y]]** {{%.*}}
}

func allocDealloc(_ size: Builtin.Word, align: Builtin.Word) {
  var ptr = Builtin.allocRaw(size, align)
  Builtin.deallocRaw(ptr, size, align)
}

func fence_test() {
  // CHECK: fence acquire
  Builtin.fence_acquire()

  // CHECK: fence singlethread acq_rel
  Builtin.fence_acqrel_singlethread()
}

func cmpxchg_test(_ ptr: Builtin.RawPointer, a: Builtin.Int32, b: Builtin.Int32) {
  // rdar://12939803 - ER: support atomic cmpxchg/xchg with pointers

  // CHECK: [[Z_RES:%.*]] = cmpxchg i32* {{.*}}, i32 {{.*}}, i32 {{.*}} acquire acquire
  // CHECK: [[Z_VAL:%.*]] = extractvalue { i32, i1 } [[Z_RES]], 0
  // CHECK: [[Z_SUCCESS:%.*]] = extractvalue { i32, i1 } [[Z_RES]], 1
  // CHECK: store i32 [[Z_VAL]], i32* {{.*}}, align 4
  // CHECK: store i1 [[Z_SUCCESS]], i1* {{.*}}, align 1
  var (z, zSuccess) = Builtin.cmpxchg_acquire_acquire_Int32(ptr, a, b)

  // CHECK: [[Y_RES:%.*]] = cmpxchg volatile i32* {{.*}}, i32 {{.*}}, i32 {{.*}} monotonic monotonic
  // CHECK: [[Y_VAL:%.*]] = extractvalue { i32, i1 } [[Y_RES]], 0
  // CHECK: [[Y_SUCCESS:%.*]] = extractvalue { i32, i1 } [[Y_RES]], 1
  // CHECK: store i32 [[Y_VAL]], i32* {{.*}}, align 4
  // CHECK: store i1 [[Y_SUCCESS]], i1* {{.*}}, align 1
  var (y, ySuccess) = Builtin.cmpxchg_monotonic_monotonic_volatile_Int32(ptr, a, b)

  // CHECK: [[X_RES:%.*]] = cmpxchg volatile i32* {{.*}}, i32 {{.*}}, i32 {{.*}} singlethread acquire monotonic
  // CHECK: [[X_VAL:%.*]] = extractvalue { i32, i1 } [[X_RES]], 0
  // CHECK: [[X_SUCCESS:%.*]] = extractvalue { i32, i1 } [[X_RES]], 1
  // CHECK: store i32 [[X_VAL]], i32* {{.*}}, align 4
  // CHECK: store i1 [[X_SUCCESS]], i1* {{.*}}, align 1
  var (x, xSuccess) = Builtin.cmpxchg_acquire_monotonic_volatile_singlethread_Int32(ptr, a, b)

  // CHECK: [[W_RES:%.*]] = cmpxchg volatile i64* {{.*}}, i64 {{.*}}, i64 {{.*}} seq_cst seq_cst
  // CHECK: [[W_VAL:%.*]] = extractvalue { i64, i1 } [[W_RES]], 0
  // CHECK: [[W_SUCCESS:%.*]] = extractvalue { i64, i1 } [[W_RES]], 1
  // CHECK: [[W_VAL_PTR:%.*]] = inttoptr i64 [[W_VAL]] to i8*
  // CHECK: store i8* [[W_VAL_PTR]], i8** {{.*}}, align 8
  // CHECK: store i1 [[W_SUCCESS]], i1* {{.*}}, align 1
  var (w, wSuccess) = Builtin.cmpxchg_seqcst_seqcst_volatile_singlethread_RawPointer(ptr, ptr, ptr)

  // CHECK: [[V_RES:%.*]] = cmpxchg weak volatile i64* {{.*}}, i64 {{.*}}, i64 {{.*}} seq_cst seq_cst
  // CHECK: [[V_VAL:%.*]] = extractvalue { i64, i1 } [[V_RES]], 0
  // CHECK: [[V_SUCCESS:%.*]] = extractvalue { i64, i1 } [[V_RES]], 1
  // CHECK: [[V_VAL_PTR:%.*]] = inttoptr i64 [[V_VAL]] to i8*
  // CHECK: store i8* [[V_VAL_PTR]], i8** {{.*}}, align 8
  // CHECK: store i1 [[V_SUCCESS]], i1* {{.*}}, align 1
  var (v, vSuccess) = Builtin.cmpxchg_seqcst_seqcst_weak_volatile_singlethread_RawPointer(ptr, ptr, ptr)
}

func atomicrmw_test(_ ptr: Builtin.RawPointer, a: Builtin.Int32,
                    ptr2: Builtin.RawPointer) {
  // CHECK: atomicrmw add i32* {{.*}}, i32 {{.*}} acquire
  var z = Builtin.atomicrmw_add_acquire_Int32(ptr, a)

  // CHECK: atomicrmw volatile max i32* {{.*}}, i32 {{.*}} monotonic
  var y = Builtin.atomicrmw_max_monotonic_volatile_Int32(ptr, a)
  
  // CHECK: atomicrmw volatile xchg i32* {{.*}}, i32 {{.*}} singlethread acquire
  var x = Builtin.atomicrmw_xchg_acquire_volatile_singlethread_Int32(ptr, a)
  
  // rdar://12939803 - ER: support atomic cmpxchg/xchg with pointers
  // CHECK: atomicrmw volatile xchg i64* {{.*}}, i64 {{.*}} singlethread acquire
  var w = Builtin.atomicrmw_xchg_acquire_volatile_singlethread_RawPointer(ptr, ptr2)

}

func addressof_test(_ a: inout Int, b: inout Bool) {
  // CHECK: bitcast i32* {{.*}} to i8*
  var ap : Builtin.RawPointer = Builtin.addressof(&a)
  // CHECK: bitcast i1* {{.*}} to i8*
  var bp : Builtin.RawPointer = Builtin.addressof(&b)
}

func fneg_test(_ half: Builtin.FPIEEE16,
               single: Builtin.FPIEEE32,
               double: Builtin.FPIEEE64)
  -> (Builtin.FPIEEE16, Builtin.FPIEEE32, Builtin.FPIEEE64)
{
  // CHECK: fsub half 0xH8000, {{%.*}}
  // CHECK: fsub float -0.000000e+00, {{%.*}}
  // CHECK: fsub double -0.000000e+00, {{%.*}}
  return (Builtin.fneg_FPIEEE16(half),
          Builtin.fneg_FPIEEE32(single),
          Builtin.fneg_FPIEEE64(double))
}

// The call to the builtins should get removed before we reach IRGen.
func testStaticReport(_ b: Bool, ptr: Builtin.RawPointer) -> () {
  Builtin.staticReport(b, b, ptr);
  return Builtin.staticReport(b, b, ptr);
}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins12testCondFail{{[_0-9a-zA-Z]*}}F(i1, i1)
func testCondFail(_ b: Bool, c: Bool) {
  // CHECK: br i1 %0, label %[[FAIL:.*]], label %[[CONT:.*]]
  Builtin.condfail(b)
  // CHECK: <label>:[[CONT]]
  // CHECK: br i1 %1, label %[[FAIL2:.*]], label %[[CONT:.*]]
  Builtin.condfail(c)
  // CHECK: <label>:[[CONT]]
  // CHECK: ret void

  // CHECK: <label>:[[FAIL]]
  // CHECK: call void @llvm.trap()
  // CHECK: unreachable

  // CHECK: <label>:[[FAIL2]]
  // CHECK: call void @llvm.trap()
  // CHECK: unreachable
}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins8testOnce{{[_0-9a-zA-Z]*}}F(i8*, i8*) {{.*}} {
// CHECK:         [[PRED_PTR:%.*]] = bitcast i8* %0 to [[WORD:i64|i32]]*
// CHECK-objc:    [[PRED:%.*]] = load {{.*}} [[WORD]]* [[PRED_PTR]]
// CHECK-objc:    [[IS_DONE:%.*]] = icmp eq [[WORD]] [[PRED]], -1
// CHECK-objc:    br i1 [[IS_DONE]], label %[[DONE:.*]], label %[[NOT_DONE:.*]]
// CHECK-objc:  [[NOT_DONE]]:
// CHECK:         call void @swift_once([[WORD]]* [[PRED_PTR]], i8* %1)
// CHECK-objc:    br label %[[DONE]]
// CHECK-objc:  [[DONE]]:
// CHECK-objc:    [[PRED:%.*]] = load {{.*}} [[WORD]]* [[PRED_PTR]]
// CHECK-objc:    [[IS_DONE:%.*]] = icmp eq [[WORD]] [[PRED]], -1
// CHECK-objc:    call void @llvm.assume(i1 [[IS_DONE]])

func testOnce(_ p: Builtin.RawPointer, f: @escaping @convention(thin) () -> ()) {
  Builtin.once(p, f)
}

class C {}
struct S {}
@objc class O {}
@objc protocol OP1 {}
@objc protocol OP2 {}
protocol P {}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins10canBeClass{{[_0-9a-zA-Z]*}}F
func canBeClass<T>(_ f: @escaping (Builtin.Int8) -> (), _: T) {
  // CHECK: call {{.*}}void {{%.*}}(i8 1
  f(Builtin.canBeClass(O.self))
  // CHECK: call {{.*}}void {{%.*}}(i8 1
  f(Builtin.canBeClass(OP1.self))
  typealias ObjCCompo = OP1 & OP2
  // CHECK: call {{.*}}void {{%.*}}(i8 1
  f(Builtin.canBeClass(ObjCCompo.self))

  // CHECK: call {{.*}}void {{%.*}}(i8 0
  f(Builtin.canBeClass(S.self))
  // CHECK: call {{.*}}void {{%.*}}(i8 1
  f(Builtin.canBeClass(C.self))
  // CHECK: call {{.*}}void {{%.*}}(i8 0
  f(Builtin.canBeClass(P.self))
  typealias MixedCompo = OP1 & P
  // CHECK: call {{.*}}void {{%.*}}(i8 0
  f(Builtin.canBeClass(MixedCompo.self))

  // CHECK: call {{.*}}void {{%.*}}(i8 2
  f(Builtin.canBeClass(T.self))
}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins15destroyPODArray{{[_0-9a-zA-Z]*}}F(i8*, i64)
// CHECK-NOT:   loop:
// CHECK:         ret void
func destroyPODArray(_ array: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.destroyArray(Int.self, array, count)
}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins18destroyNonPODArray{{[_0-9a-zA-Z]*}}F(i8*, i64) {{.*}} {
// CHECK:       iter:
// CHECK:       loop:
// CHECK:         call {{.*}} @swift_rt_swift_release
// CHECK:         br label %iter
func destroyNonPODArray(_ array: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.destroyArray(C.self, array, count)
}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins15destroyGenArrayyBp_Bw5countxtlF(i8*, i64, %swift.opaque* noalias nocapture, %swift.type* %T)
// CHECK-NOT:   loop:
// CHECK:         call void %destroyArray
func destroyGenArray<T>(_ array: Builtin.RawPointer, count: Builtin.Word, _: T) {
  Builtin.destroyArray(T.self, array, count)
}


// CHECK-LABEL: define hidden {{.*}}void @_T08builtins12copyPODArray{{[_0-9a-zA-Z]*}}F(i8*, i8*, i64)
// CHECK:         mul nuw i64 4, %2
// CHECK:         call void @llvm.memcpy.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 4, i1 false)
// CHECK:         mul nuw i64 4, %2
// CHECK:         call void @llvm.memmove.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 4, i1 false)
// CHECK:         mul nuw i64 4, %2
// CHECK:         call void @llvm.memmove.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 4, i1 false)
func copyPODArray(_ dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(Int.self, dest, src, count)
  Builtin.takeArrayFrontToBack(Int.self, dest, src, count)
  Builtin.takeArrayBackToFront(Int.self, dest, src, count)
}


// CHECK-LABEL: define hidden {{.*}}void @_T08builtins11copyBTArray{{[_0-9a-zA-Z]*}}F(i8*, i8*, i64) {{.*}} {
// CHECK:       iter:
// CHECK:       loop:
// CHECK:         call {{.*}} @swift_rt_swift_retain
// CHECK:         br label %iter
// CHECK:         mul nuw i64 8, %2
// CHECK:         call void @llvm.memmove.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 8, i1 false)
// CHECK:         mul nuw i64 8, %2
// CHECK:         call void @llvm.memmove.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 8, i1 false)
func copyBTArray(_ dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(C.self, dest, src, count)
  Builtin.takeArrayFrontToBack(C.self, dest, src, count)
  Builtin.takeArrayBackToFront(C.self, dest, src, count)
}

struct W { weak var c: C? }

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins15copyNonPODArray{{[_0-9a-zA-Z]*}}F(i8*, i8*, i64) {{.*}} {
// CHECK:       iter:
// CHECK:       loop:
// CHECK:         swift_weakCopyInit
// CHECK:       iter{{.*}}:
// CHECK:       loop{{.*}}:
// CHECK:         swift_weakTakeInit
// CHECK:       iter{{.*}}:
// CHECK:       loop{{.*}}:
// CHECK:         swift_weakTakeInit
func copyNonPODArray(_ dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(W.self, dest, src, count)
  Builtin.takeArrayFrontToBack(W.self, dest, src, count)
  Builtin.takeArrayBackToFront(W.self, dest, src, count)
}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins12copyGenArray{{[_0-9a-zA-Z]*}}F(i8*, i8*, i64, %swift.opaque* noalias nocapture, %swift.type* %T)
// CHECK-NOT:   loop:
// CHECK:         call %swift.opaque* %initializeArrayWithCopy
// CHECK-NOT:   loop:
// CHECK:         call %swift.opaque* %initializeArrayWithTakeFrontToBack
// CHECK-NOT:   loop:
// CHECK:         call %swift.opaque* %initializeArrayWithTakeBackToFront
func copyGenArray<T>(_ dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word, _: T) {
  Builtin.copyArray(T.self, dest, src, count)
  Builtin.takeArrayFrontToBack(T.self, dest, src, count)
  Builtin.takeArrayBackToFront(T.self, dest, src, count)
}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins24conditionallyUnreachableyyF
// CHECK-NEXT:  entry
// CHECK-NEXT:    unreachable
func conditionallyUnreachable() {
  Builtin.conditionallyUnreachable()
}

struct Abc {
	var value : Builtin.Word
}

// CHECK-LABEL define hidden {{.*}}@_T08builtins22assumeNonNegative_testBwAA3AbcVzF
func assumeNonNegative_test(_ x: inout Abc) -> Builtin.Word {
  // CHECK: load {{.*}}, !range ![[R:[0-9]+]]
  return Builtin.assumeNonNegative_Word(x.value)
}

@inline(never)
func return_word(_ x: Builtin.Word) -> Builtin.Word {
	return x
}

// CHECK-LABEL define hidden {{.*}}@_T08builtins23assumeNonNegative_test2BwAA3AbcVzF
func assumeNonNegative_test2(_ x: Builtin.Word) -> Builtin.Word {
  // CHECK: call {{.*}}, !range ![[R]]
  return Builtin.assumeNonNegative_Word(return_word(x))
}

struct Empty {}
struct Pair { var i: Int, b: Bool }

// CHECK-LABEL: define hidden {{.*}}i64 @_T08builtins15zeroInitializerAA5EmptyV_AA4PairVtyF() {{.*}} {
// CHECK:  [[ALLOCA:%.*]] = alloca { i64 }
// CHECK:  bitcast
// CHECK:  lifetime.start
// CHECK:  [[EMPTYPAIR:%.*]] = bitcast { i64 }* [[ALLOCA]]
// CHECK:  [[PAIR:%.*]] = getelementptr inbounds {{.*}} [[EMPTYPAIR]], i32 0, i32 0
// CHECK:  [[FLDI:%.*]] = getelementptr inbounds {{.*}} [[PAIR]], i32 0, i32 0
// CHECK:  store i32 0, i32* [[FLDI]]
// CHECK:  [[FLDB:%.*]] = getelementptr inbounds {{.*}} [[PAIR]], i32 0, i32 1
// CHECK:  store i1 false, i1* [[FLDB]]
// CHECK:  [[RET:%.*]] = getelementptr inbounds {{.*}} [[ALLOCA]], i32 0, i32 0
// CHECK:  [[RES:%.*]] = load i64, i64* [[RET]]
// CHECK:  ret i64 [[RES]]
func zeroInitializer() -> (Empty, Pair) {
  return (Builtin.zeroInitializer(), Builtin.zeroInitializer())
}

// CHECK-LABEL: define hidden {{.*}}i64 @_T08builtins20zeroInitializerTupleAA5EmptyV_AA4PairVtyF() {{.*}} {
// CHECK:  [[ALLOCA:%.*]] = alloca { i64 }
// CHECK:  bitcast
// CHECK:  lifetime.start
// CHECK:  [[EMPTYPAIR:%.*]] = bitcast { i64 }* [[ALLOCA]]
// CHECK:  [[PAIR:%.*]] = getelementptr inbounds {{.*}} [[EMPTYPAIR]], i32 0, i32 0
// CHECK:  [[FLDI:%.*]] = getelementptr inbounds {{.*}} [[PAIR]], i32 0, i32 0
// CHECK:  store i32 0, i32* [[FLDI]]
// CHECK:  [[FLDB:%.*]] = getelementptr inbounds {{.*}} [[PAIR]], i32 0, i32 1
// CHECK:  store i1 false, i1* [[FLDB]]
// CHECK:  [[RET:%.*]] = getelementptr inbounds {{.*}} [[ALLOCA]], i32 0, i32 0
// CHECK:  [[RES:%.*]] = load i64, i64* [[RET]]
// CHECK:  ret i64 [[RES]]
func zeroInitializerTuple() -> (Empty, Pair) {
  return Builtin.zeroInitializer()
}

// CHECK-LABEL: define hidden {{.*}}void @_T08builtins20zeroInitializerEmptyyyF() {{.*}} {
// CHECK:         ret void
func zeroInitializerEmpty() {
  return Builtin.zeroInitializer()
}

// ----------------------------------------------------------------------------
// isUnique variants
// ----------------------------------------------------------------------------

// CHECK: define hidden {{.*}}void @_T08builtins26acceptsBuiltinNativeObjectyBoSgzF([[BUILTIN_NATIVE_OBJECT_TY:%.*]]* nocapture dereferenceable({{.*}})) {{.*}} {
func acceptsBuiltinNativeObject(_ ref: inout Builtin.NativeObject?) {}

// native
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins8isUniqueBi1_BoSgzF({{%.*}}* nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast [[BUILTIN_NATIVE_OBJECT_TY]]* %0 to %swift.refcounted**
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %1
// CHECK-NEXT: call i1 @swift_isUniquelyReferenced_native(%swift.refcounted* %2)
// CHECK-NEXT: ret i1 %3
func isUnique(_ ref: inout Builtin.NativeObject?) -> Bool {
  return Builtin.isUnique(&ref)
}

// native nonNull
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins8isUniqueBi1_BozF(%swift.refcounted** nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %0
// CHECK-NEXT: call i1 @swift_rt_swift_isUniquelyReferenced_nonNull_native(%swift.refcounted* %1)
// CHECK-NEXT: ret i1 %2
func isUnique(_ ref: inout Builtin.NativeObject) -> Bool {
  return Builtin.isUnique(&ref)
}

// native pinned
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins16isUniqueOrPinnedBi1_BoSgzF({{%.*}}* nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast [[BUILTIN_NATIVE_OBJECT_TY]]* %0 to %swift.refcounted**
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %1
// CHECK-NEXT: call i1 @swift_rt_swift_isUniquelyReferencedOrPinned_native(%swift.refcounted* %2)
// CHECK-NEXT: ret i1 %3
func isUniqueOrPinned(_ ref: inout Builtin.NativeObject?) -> Bool {
  return Builtin.isUniqueOrPinned(&ref)
}

// native pinned nonNull
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins16isUniqueOrPinnedBi1_BozF(%swift.refcounted** nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %0
// CHECK-NEXT: call i1 @swift_rt_swift_isUniquelyReferencedOrPinned_nonNull_native(%swift.refcounted* %1)
// CHECK-NEXT: ret i1 %2
func isUniqueOrPinned(_ ref: inout Builtin.NativeObject) -> Bool {
  return Builtin.isUniqueOrPinned(&ref)
}

// CHECK: define hidden {{.*}}void @_T08builtins27acceptsBuiltinUnknownObjectyBOSgzF([[BUILTIN_UNKNOWN_OBJECT_TY:%.*]]* nocapture dereferenceable({{.*}})) {{.*}} {
func acceptsBuiltinUnknownObject(_ ref: inout Builtin.UnknownObject?) {}

// ObjC
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins8isUniqueBi1_BOSgzF({{%.*}}* nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast [[BUILTIN_UNKNOWN_OBJECT_TY]]* %0 to %objc_object**
// CHECK-NEXT: load %objc_object*, %objc_object** %1
// CHECK-NEXT: call i1 @swift_isUniquelyReferencedNonObjC(%objc_object* %2)
// CHECK-NEXT: ret i1 %3
func isUnique(_ ref: inout Builtin.UnknownObject?) -> Bool {
  return Builtin.isUnique(&ref)
}

// ObjC nonNull
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins8isUniqueBi1_BOzF(%objc_object** nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %objc_object*, %objc_object** %0
// CHECK-NEXT: call i1 @swift_isUniquelyReferencedNonObjC_nonNull(%objc_object* %1)
// CHECK-NEXT: ret i1 %2
func isUnique(_ ref: inout Builtin.UnknownObject) -> Bool {
  return Builtin.isUnique(&ref)
}

// ObjC pinned nonNull
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins16isUniqueOrPinnedBi1_BOzF(%objc_object** nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %objc_object*, %objc_object** %0
// CHECK-NEXT: call i1 @swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(%objc_object* %1)
// CHECK-NEXT: ret i1 %2
func isUniqueOrPinned(_ ref: inout Builtin.UnknownObject) -> Bool {
  return Builtin.isUniqueOrPinned(&ref)
}

// BridgeObject nonNull
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins8isUniqueBi1_BbzF(%swift.bridge** nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %swift.bridge*, %swift.bridge** %0
// CHECK-NEXT: call i1 @swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(%swift.bridge* %1)
// CHECK-NEXT: ret i1 %2
func isUnique(_ ref: inout Builtin.BridgeObject) -> Bool {
  return Builtin.isUnique(&ref)
}

// Bridge pinned nonNull
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins16isUniqueOrPinnedBi1_BbzF(%swift.bridge** nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %swift.bridge*, %swift.bridge** %0
// CHECK-NEXT: call i1 @swift_isUniquelyReferencedOrPinnedNonObjC_nonNull_bridgeObject(%swift.bridge* %1)
// CHECK-NEXT: ret i1 %2
func isUniqueOrPinned(_ ref: inout Builtin.BridgeObject) -> Bool {
  return Builtin.isUniqueOrPinned(&ref)
}

// BridgeObject nonNull
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins15isUnique_nativeBi1_BbzF(%swift.bridge** nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast %swift.bridge** %0 to %swift.refcounted**
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %1
// CHECK-NEXT: call i1 @swift_rt_swift_isUniquelyReferenced_nonNull_native(%swift.refcounted* %2)
// CHECK-NEXT: ret i1 %3
func isUnique_native(_ ref: inout Builtin.BridgeObject) -> Bool {
  return Builtin.isUnique_native(&ref)
}

// Bridge pinned nonNull
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins23isUniqueOrPinned_nativeBi1_BbzF(%swift.bridge** nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast %swift.bridge** %0 to %swift.refcounted**
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %1
// CHECK-NEXT: call i1 @swift_rt_swift_isUniquelyReferencedOrPinned_nonNull_native(%swift.refcounted* %2)
// CHECK-NEXT: ret i1 %3
func isUniqueOrPinned_native(_ ref: inout Builtin.BridgeObject) -> Bool {
  return Builtin.isUniqueOrPinned_native(&ref)
}

// ImplicitlyUnwrappedOptional argument to isUnique.
// CHECK-LABEL: define hidden {{.*}}i1 @_T08builtins11isUniqueIUOBi1_BoSgzF(%{{.*}}* nocapture dereferenceable({{.*}})) {{.*}} {
// CHECK-NEXT: entry:
// CHECK: call i1 @swift_isUniquelyReferenced_native(%swift.refcounted*
// CHECK: ret i1
func isUniqueIUO(_ ref: inout Builtin.NativeObject?) -> Bool {
  var iuo : Builtin.NativeObject! = ref
  return Builtin.isUnique(&iuo)
}

// CHECK-LABEL: define {{.*}} @{{.*}}generic_ispod_test
func generic_ispod_test<T>(_: T) {
  // CHECK:      [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T:%.*]], i32 18
  // CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]]
  // CHECK-NEXT: [[FLAGS:%.*]] = ptrtoint i8* [[T1]] to i64
  // CHECK-NEXT: [[ISNOTPOD:%.*]] = and i64 [[FLAGS]], 65536
  // CHECK-NEXT: [[ISPOD:%.*]] = icmp eq i64 [[ISNOTPOD]], 0
  // CHECK-NEXT: store i1 [[ISPOD]], i1* [[S:%.*]]
  var s = Builtin.ispod(T.self)
}

// CHECK-LABEL: define {{.*}} @{{.*}}ispod_test
func ispod_test() {
  // CHECK: store i1 true, i1*
  // CHECK: store i1 false, i1*
  var t = Builtin.ispod(Int.self)
  var f = Builtin.ispod(Builtin.NativeObject)
}

// CHECK-LABEL: define {{.*}} @{{.*}}generic_unsafeGuaranteed_test
// CHECK:  call void @{{.*}}swift_{{.*}}etain({{.*}}* %0)
// CHECK:  call void @{{.*}}swift_{{.*}}elease({{.*}}* %0)
// CHECK:  ret {{.*}}* %0
func generic_unsafeGuaranteed_test<T: AnyObject>(_ t : T) -> T {
  let (g, _) = Builtin.unsafeGuaranteed(t)
  return g
}

// CHECK-LABEL: define {{.*}} @{{.*}}unsafeGuaranteed_test
// CHECK:  [[LOCAL:%.*]] = alloca %swift.refcounted*
// CHECK:  call void @swift_rt_swift_retain(%swift.refcounted* %0)
// CHECK:  store %swift.refcounted* %0, %swift.refcounted** [[LOCAL]]
// CHECK:  call void @swift_rt_swift_release(%swift.refcounted* %0)
// CHECK:  ret %swift.refcounted* %0
func unsafeGuaranteed_test(_ x: Builtin.NativeObject) -> Builtin.NativeObject {
  var (g,t) = Builtin.unsafeGuaranteed(x)
  Builtin.unsafeGuaranteedEnd(t)
  return g
}

// CHECK-LABEL: define {{.*}} @{{.*}}unsafeGuaranteedEnd_test
// CHECK-NEXT: {{.*}}:
// CHECK-NEXT: ret void
func unsafeGuaranteedEnd_test(_ x: Builtin.Int8) {
  Builtin.unsafeGuaranteedEnd(x)
}

// CHECK-LABEL: define {{.*}} @{{.*}}atomicload
func atomicload(_ p: Builtin.RawPointer) {
  // CHECK: [[A:%.*]] = load atomic i8*, i8** {{%.*}} unordered, align 8
  let a: Builtin.RawPointer = Builtin.atomicload_unordered_RawPointer(p)
  // CHECK: [[B:%.*]] = load atomic i32, i32* {{%.*}} singlethread monotonic, align 4
  let b: Builtin.Int32 = Builtin.atomicload_monotonic_singlethread_Int32(p)
  // CHECK: [[C:%.*]] = load atomic volatile i64, i64* {{%.*}} singlethread acquire, align 8
  let c: Builtin.Int64 =
    Builtin.atomicload_acquire_volatile_singlethread_Int64(p)
  // CHECK: [[D0:%.*]] = load atomic volatile i32, i32* {{%.*}} seq_cst, align 4
  // CHECK: [[D:%.*]] = bitcast i32 [[D0]] to float
  let d: Builtin.FPIEEE32 = Builtin.atomicload_seqcst_volatile_FPIEEE32(p)

  // CHECK: store atomic i8* [[A]], i8** {{%.*}} unordered, align 8
  Builtin.atomicstore_unordered_RawPointer(p, a)
  // CHECK: store atomic i32 [[B]], i32* {{%.*}} singlethread monotonic, align 4
  Builtin.atomicstore_monotonic_singlethread_Int32(p, b)
  // CHECK: store atomic volatile i64 [[C]], i64* {{%.*}} singlethread release, align 8
  Builtin.atomicstore_release_volatile_singlethread_Int64(p, c)
  // CHECK: [[D1:%.*]] = bitcast float [[D]] to i32
  // CHECK: store atomic volatile i32 [[D1]], i32* {{.*}} seq_cst, align 4
  Builtin.atomicstore_seqcst_volatile_FPIEEE32(p, d)
}

// CHECK: ![[R]] = !{i64 0, i64 9223372036854775807}

