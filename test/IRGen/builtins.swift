// RUN: %target-swift-frontend -parse-stdlib -primary-file %s -emit-ir -o - -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: CPU=x86_64

import Swift

// CHECK-DAG: [[REFCOUNT:%swift.refcounted.*]] = type
// CHECK-DAG: [[X:%C8builtins1X]] = type
// CHECK-DAG: [[Y:%C8builtins1Y]] = type

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

func gep_test(ptr: Builtin.RawPointer, offset: Builtin.Int64)
   -> Builtin.RawPointer {
  return Builtin.gep_Int64(ptr, offset)
  // CHECK: getelementptr inbounds i8, i8*
}

// CHECK: define hidden i64 @_TF8builtins9load_test
func load_test(ptr: Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[CASTPTR:%.*]] = bitcast i8* [[PTR:%.*]] to i64*
  // CHECK-NEXT: load i64, i64* [[CASTPTR]]
  // CHECK: ret
  return Builtin.load(ptr)
}

// CHECK: define hidden void @_TF8builtins11assign_test
func assign_test(value: Builtin.Int64, ptr: Builtin.RawPointer) {
  Builtin.assign(value, ptr)
  // CHECK: ret
}

// CHECK: define hidden %swift.refcounted* @_TF8builtins16load_object_test
func load_object_test(ptr: Builtin.RawPointer) -> Builtin.NativeObject {
  // CHECK: [[T0:%.*]] = load [[REFCOUNT]]*, [[REFCOUNT]]**
  // CHECK: call void @swift_retain_noresult([[REFCOUNT]]* [[T0]])
  // CHECK: ret [[REFCOUNT]]* [[T0]]
  return Builtin.load(ptr)
}

// CHECK: define hidden void @_TF8builtins18assign_object_test
func assign_object_test(value: Builtin.NativeObject, ptr: Builtin.RawPointer) {
  Builtin.assign(value, ptr)
}

// CHECK: define hidden void @_TF8builtins16init_object_test
func init_object_test(value: Builtin.NativeObject, ptr: Builtin.RawPointer) {
  // CHECK: [[DEST:%.*]] = bitcast i8* {{%.*}} to %swift.refcounted**
  // CHECK-NEXT: store [[REFCOUNT]]* {{%.*}}, [[REFCOUNT]]** [[DEST]]
  Builtin.initialize(value, ptr)
}

func cast_test(inout ptr: Builtin.RawPointer, inout i8: Builtin.Int8,
               inout i64: Builtin.Int64, inout f: Builtin.FPIEEE32,
               inout d: Builtin.FPIEEE64
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

func intrinsic_test(inout i32: Builtin.Int32, inout i16: Builtin.Int16) {
  i32 = Builtin.int_bswap_Int32(i32) // CHECK: llvm.bswap.i32(

  i16 = Builtin.int_bswap_Int16(i16) // CHECK: llvm.bswap.i16(
  
  var x = Builtin.int_sadd_with_overflow_Int16(i16, i16) // CHECK: call { i16, i1 } @llvm.sadd.with.overflow.i16(
  
  Builtin.int_trap() // CHECK: llvm.trap()
}

// CHECK: define hidden void @_TF8builtins19sizeof_alignof_testFT_T_()
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

// CHECK: define hidden void @_TF8builtins27generic_sizeof_alignof_testU__FT_T_(
func generic_sizeof_alignof_test<T>() {
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

// CHECK: define hidden void @_TF8builtins21generic_strideof_testU__FT_T_(
func generic_strideof_test<T>() {
  // CHECK:      [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T:%.*]], i32 19
  // CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]]
  // CHECK-NEXT: [[STRIDE:%.*]] = ptrtoint i8* [[T1]] to i64
  // CHECK-NEXT: store i64 [[STRIDE]], i64* [[S:%.*]]
  var s = Builtin.strideof(T.self)
}

// CHECK: define hidden void @_TF8builtins29generic_strideof_nonzero_testU__FT_T_(
func generic_strideof_nonzero_test<T>() {
  // CHECK:      [[T0:%.*]] = getelementptr inbounds i8*, i8** [[T:%.*]], i32 19
  // CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]]
  // CHECK-NEXT: [[STRIDE:%.*]] = ptrtoint i8* [[T1]] to i64
  // CHECK-NEXT: [[CMP:%.*]] = icmp eq i64 [[STRIDE]], 0
  // CHECK-NEXT: [[SELECT:%.*]] = select i1 [[CMP]], i64 1, i64 [[STRIDE]]
  // CHECK-NEXT: store i64 [[SELECT]], i64* [[S:%.*]]
  var s = Builtin.strideof_nonzero(T.self)
}

class X {}

class Y {}
func move(ptr: Builtin.RawPointer) {
  var temp : Y = Builtin.take(ptr)
  // CHECK:      define hidden void @_TF8builtins4move
  // CHECK:        [[SRC:%.*]] = bitcast i8* {{%.*}} to [[Y]]**
  // CHECK-NEXT:   [[VAL:%.*]] = load [[Y]]*, [[Y]]** [[SRC]]
  // CHECK-NEXT:   store [[Y]]* [[VAL]], [[Y]]** {{%.*}}
}

func allocDealloc(size: Builtin.Word, align: Builtin.Word) {
  var ptr = Builtin.allocRaw(size, align)
  Builtin.deallocRaw(ptr, size, align)
}

func fence_test() {
  // CHECK: fence acquire
  Builtin.fence_acquire()

  // CHECK: fence singlethread acq_rel
  Builtin.fence_acqrel_singlethread()
}

func cmpxchg_test(ptr: Builtin.RawPointer, a: Builtin.Int32, b: Builtin.Int32) {
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

func atomicrmw_test(ptr: Builtin.RawPointer, a: Builtin.Int32,
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

func addressof_test(inout a: Int, inout b: Bool) {
  // CHECK: bitcast i32* {{.*}} to i8*
  var ap : Builtin.RawPointer = Builtin.addressof(&a)
  // CHECK: bitcast i1* {{.*}} to i8*
  var bp : Builtin.RawPointer = Builtin.addressof(&b)
}

func fneg_test(half: Builtin.FPIEEE16,
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
func testStaticReport(b: Bool, ptr: Builtin.RawPointer) -> () {
  Builtin.staticReport(b, b, ptr);
  return Builtin.staticReport(b, b, ptr);
}

// CHECK-LABEL: define hidden void @_TF8builtins12testCondFail{{.*}}(i1, i1)
func testCondFail(b: Bool, c: Bool) {
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

// CHECK-LABEL: define hidden void @_TF8builtins8testOnce{{.*}}(i8*, i8*, %swift.refcounted*) {
// CHECK:         [[PRED_PTR:%.*]] = bitcast i8* %0 to i64*
// CHECK:         call void @swift_once(i64* [[PRED_PTR]], i8* %1, %swift.refcounted* %2)
func testOnce(p: Builtin.RawPointer, f: () -> ()) {
  Builtin.once(p, f)
}

class C {}
struct S {}
@objc class O {}
@objc protocol OP1 {}
@objc protocol OP2 {}
protocol P {}

// CHECK-LABEL: define hidden void @_TF8builtins10canBeClass
func canBeClass<T>(f: (Builtin.Int8) -> ()) {
  // CHECK: call void {{%.*}}(i8 1
  f(Builtin.canBeClass(O.self))
  // CHECK: call void {{%.*}}(i8 1
  f(Builtin.canBeClass(OP1.self))
  typealias ObjCCompo = protocol<OP1, OP2>
  // CHECK: call void {{%.*}}(i8 1
  f(Builtin.canBeClass(ObjCCompo.self))

  // CHECK: call void {{%.*}}(i8 0
  f(Builtin.canBeClass(S.self))
  // CHECK: call void {{%.*}}(i8 1
  f(Builtin.canBeClass(C.self))
  // CHECK: call void {{%.*}}(i8 0
  f(Builtin.canBeClass(P.self))
  typealias MixedCompo = protocol<OP1, P>
  // CHECK: call void {{%.*}}(i8 0
  f(Builtin.canBeClass(MixedCompo.self))

  // CHECK: call void {{%.*}}(i8 2
  f(Builtin.canBeClass(T.self))
}

// CHECK-LABEL: define hidden void @_TF8builtins15destroyPODArray{{.*}}(i8*, i64)
// CHECK-NOT:   loop:
// CHECK:         ret void
func destroyPODArray(array: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.destroyArray(Int.self, array, count)
}

// CHECK-LABEL: define hidden void @_TF8builtins18destroyNonPODArray{{.*}}(i8*, i64) {
// CHECK:       iter:
// CHECK:       loop:
// CHECK:         call {{.*}} @swift_release
// CHECK:         br label %iter
func destroyNonPODArray(array: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.destroyArray(C.self, array, count)
}

// CHECK-LABEL: define hidden void @_TF8builtins15destroyGenArray{{.*}}(i8*, i64, %swift.type* %T)
// CHECK-NOT:   loop:
// CHECK:         call void %destroyArray
func destroyGenArray<T>(array: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.destroyArray(T.self, array, count)
}


// CHECK-LABEL: define hidden void @_TF8builtins12copyPODArray{{.*}}(i8*, i8*, i64)
// CHECK:         mul nuw i64 4, %2
// CHECK:         call void @llvm.memcpy.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 4, i1 false)
// CHECK:         mul nuw i64 4, %2
// CHECK:         call void @llvm.memmove.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 4, i1 false)
// CHECK:         mul nuw i64 4, %2
// CHECK:         call void @llvm.memmove.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 4, i1 false)
func copyPODArray(dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(Int.self, dest, src, count)
  Builtin.takeArrayFrontToBack(Int.self, dest, src, count)
  Builtin.takeArrayBackToFront(Int.self, dest, src, count)
}


// CHECK-LABEL: define hidden void @_TF8builtins11copyBTArray{{.*}}(i8*, i8*, i64) {
// CHECK:       iter:
// CHECK:       loop:
// CHECK:         call {{.*}} @swift_retain_noresult
// CHECK:         br label %iter
// CHECK:         mul nuw i64 8, %2
// CHECK:         call void @llvm.memmove.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 8, i1 false)
// CHECK:         mul nuw i64 8, %2
// CHECK:         call void @llvm.memmove.p0i8.p0i8.i64(i8* {{.*}}, i8* {{.*}}, i64 {{.*}}, i32 8, i1 false)
func copyBTArray(dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(C.self, dest, src, count)
  Builtin.takeArrayFrontToBack(C.self, dest, src, count)
  Builtin.takeArrayBackToFront(C.self, dest, src, count)
}

struct W { weak var c: C? }

// CHECK-LABEL: define hidden void @_TF8builtins15copyNonPODArray{{.*}}(i8*, i8*, i64) {
// CHECK:       iter:
// CHECK:       loop:
// CHECK:         swift_weakCopyInit
// CHECK:       iter{{.*}}:
// CHECK:       loop{{.*}}:
// CHECK:         swift_weakTakeInit
// CHECK:       iter{{.*}}:
// CHECK:       loop{{.*}}:
// CHECK:         swift_weakTakeInit
func copyNonPODArray(dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(W.self, dest, src, count)
  Builtin.takeArrayFrontToBack(W.self, dest, src, count)
  Builtin.takeArrayBackToFront(W.self, dest, src, count)
}

// CHECK-LABEL: define hidden void @_TF8builtins12copyGenArray{{.*}}(i8*, i8*, i64, %swift.type* %T) {
// CHECK-NOT:   loop:
// CHECK:         call %swift.opaque* %initializeArrayWithCopy
// CHECK-NOT:   loop:
// CHECK:         call %swift.opaque* %initializeArrayWithTakeFrontToBack
// CHECK-NOT:   loop:
// CHECK:         call %swift.opaque* %initializeArrayWithTakeBackToFront
func copyGenArray<T>(dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(T.self, dest, src, count)
  Builtin.takeArrayFrontToBack(T.self, dest, src, count)
  Builtin.takeArrayBackToFront(T.self, dest, src, count)
}

// CHECK-LABEL: define hidden void @_TF8builtins24conditionallyUnreachableFT_T_
// CHECK-NEXT:  entry
// CHECK-NEXT:    unreachable
func conditionallyUnreachable() {
  Builtin.conditionallyUnreachable()
}

struct Abc {
	var value : Builtin.Word
}

// CHECK-LABEL define hidden @_TF8builtins22assumeNonNegative_testFRVS_3AbcBw
func assumeNonNegative_test(inout x: Abc) -> Builtin.Word {
  // CHECK: load {{.*}}, !range ![[R:[0-9]+]]
  return Builtin.assumeNonNegative_Word(x.value)
}

@inline(never)
func return_word(x: Builtin.Word) -> Builtin.Word {
	return x
}

// CHECK-LABEL define hidden @_TF8builtins23assumeNonNegative_test2FRVS_3AbcBw
func assumeNonNegative_test2(x: Builtin.Word) -> Builtin.Word {
  // CHECK: call {{.*}}, !range ![[R]]
  return Builtin.assumeNonNegative_Word(return_word(x))
}

struct Empty {}
struct Pair { var i: Int, b: Bool }

// CHECK-LABEL: define hidden { i32, i1 } @_TF8builtins15zeroInitializerFT_TVS_5EmptyVS_4Pair_() {
// CHECK:         ret { i32, i1 } zeroinitializer
func zeroInitializer() -> (Empty, Pair) {
  return (Builtin.zeroInitializer(), Builtin.zeroInitializer())
}

// CHECK-LABEL: define hidden { i32, i1 } @_TF8builtins20zeroInitializerTupleFT_TVS_5EmptyVS_4Pair_() {
// CHECK:         ret { i32, i1 } zeroinitializer
func zeroInitializerTuple() -> (Empty, Pair) {
  return Builtin.zeroInitializer()
}

// CHECK-LABEL: define hidden void @_TF8builtins20zeroInitializerEmptyFT_T_() {
// CHECK:         ret void
func zeroInitializerEmpty() {
  return Builtin.zeroInitializer()
}

// ----------------------------------------------------------------------------
// isUnique variants
// ----------------------------------------------------------------------------

// CHECK: define hidden void @_TF8builtins26acceptsBuiltinNativeObjectFRGSqBo_T_([[BUILTIN_NATIVE_OBJECT_TY:%.*]]*) {
func acceptsBuiltinNativeObject(inout ref: Builtin.NativeObject?) {}

// native
// CHECK-LABEL: define hidden i1 @_TF8builtins8isUniqueFRGSqBo_Bi1_({{%.*}}*) {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast [[BUILTIN_NATIVE_OBJECT_TY]]* %0 to %swift.refcounted**
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %1
// CHECK-NEXT: call i1 @_swift_isUniquelyReferenced_native(%swift.refcounted* %2)
// CHECK-NEXT: ret i1 %3
func isUnique(inout ref: Builtin.NativeObject?) -> Bool {
  return Builtin.isUnique(&ref)
}

// native nonNull
// CHECK-LABEL: define hidden i1 @_TF8builtins8isUniqueFRBoBi1_(%swift.refcounted**) {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %0
// CHECK-NEXT: call i1 @_swift_isUniquelyReferenced_nonNull_native(%swift.refcounted* %1)
// CHECK-NEXT: ret i1 %2
func isUnique(inout ref: Builtin.NativeObject) -> Bool {
  return Builtin.isUnique(&ref)
}

// native pinned
// CHECK-LABEL: define hidden i1 @_TF8builtins16isUniqueOrPinnedFRGSqBo_Bi1_({{%.*}}*) {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast [[BUILTIN_NATIVE_OBJECT_TY]]* %0 to %swift.refcounted**
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %1
// CHECK-NEXT: call i1 @_swift_isUniquelyReferencedOrPinned_native(%swift.refcounted* %2)
// CHECK-NEXT: ret i1 %3
func isUniqueOrPinned(inout ref: Builtin.NativeObject?) -> Bool {
  return Builtin.isUniqueOrPinned(&ref)
}

// native pinned nonNull
// CHECK-LABEL: define hidden i1 @_TF8builtins16isUniqueOrPinnedFRBoBi1_(%swift.refcounted**) {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %0
// CHECK-NEXT: call i1 @_swift_isUniquelyReferencedOrPinned_nonNull_native(%swift.refcounted* %1)
// CHECK-NEXT: ret i1 %2
func isUniqueOrPinned(inout ref: Builtin.NativeObject) -> Bool {
  return Builtin.isUniqueOrPinned(&ref)
}

// CHECK: define hidden void @_TF8builtins27acceptsBuiltinUnknownObjectFRGSqBO_T_([[BUILTIN_UNKNOWN_OBJECT_TY:%.*]]*) {
func acceptsBuiltinUnknownObject(inout ref: Builtin.UnknownObject?) {}

// ObjC
// CHECK-LABEL: define hidden i1 @_TF8builtins8isUniqueFRGSqBO_Bi1_({{%.*}}*) {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast [[BUILTIN_UNKNOWN_OBJECT_TY]]* %0 to %objc_object**
// CHECK-NEXT: load %objc_object*, %objc_object** %1
// CHECK-NEXT: call i1 @_swift_isUniquelyReferencedNonObjC(%objc_object* %2)
// CHECK-NEXT: ret i1 %3
func isUnique(inout ref: Builtin.UnknownObject?) -> Bool {
  return Builtin.isUnique(&ref)
}

// ObjC nonNull
// CHECK-LABEL: define hidden i1 @_TF8builtins8isUniqueFRBOBi1_(%objc_object**) {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %objc_object*, %objc_object** %0
// CHECK-NEXT: call i1 @_swift_isUniquelyReferencedNonObjC_nonNull(%objc_object* %1)
// CHECK-NEXT: ret i1 %2
func isUnique(inout ref: Builtin.UnknownObject) -> Bool {
  return Builtin.isUnique(&ref)
}

// ObjC pinned nonNull
// CHECK-LABEL: define hidden i1 @_TF8builtins16isUniqueOrPinnedFRBOBi1_(%objc_object**) {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %objc_object*, %objc_object** %0
// CHECK-NEXT: call i1 @_swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(%objc_object* %1)
// CHECK-NEXT: ret i1 %2
func isUniqueOrPinned(inout ref: Builtin.UnknownObject) -> Bool {
  return Builtin.isUniqueOrPinned(&ref)
}

// BridgeObject nonNull
// CHECK-LABEL: define hidden i1 @_TF8builtins8isUniqueFRBbBi1_(%swift.bridge**) {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %swift.bridge*, %swift.bridge** %0
// CHECK-NEXT: call i1 @_swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(%swift.bridge* %1)
// CHECK-NEXT: ret i1 %2
func isUnique(inout ref: Builtin.BridgeObject) -> Bool {
  return Builtin.isUnique(&ref)
}

// Bridge pinned nonNull
// CHECK-LABEL: define hidden i1 @_TF8builtins16isUniqueOrPinnedFRBbBi1_(%swift.bridge**) {
// CHECK-NEXT: entry:
// CHECK-NEXT: load %swift.bridge*, %swift.bridge** %0
// CHECK-NEXT: call i1 @_swift_isUniquelyReferencedOrPinnedNonObjC_nonNull_bridgeObject(%swift.bridge* %1)
// CHECK-NEXT: ret i1 %2
func isUniqueOrPinned(inout ref: Builtin.BridgeObject) -> Bool {
  return Builtin.isUniqueOrPinned(&ref)
}

// BridgeObject nonNull
// CHECK-LABEL: define hidden i1 @_TF8builtins15isUnique_nativeFRBbBi1_(%swift.bridge**) {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast %swift.bridge** %0 to %swift.refcounted**
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %1
// CHECK-NEXT: call i1 @_swift_isUniquelyReferenced_nonNull_native(%swift.refcounted* %2)
// CHECK-NEXT: ret i1 %3
func isUnique_native(inout ref: Builtin.BridgeObject) -> Bool {
  return Builtin.isUnique_native(&ref)
}

// Bridge pinned nonNull
// CHECK-LABEL: define hidden i1 @_TF8builtins23isUniqueOrPinned_nativeFRBbBi1_(%swift.bridge**) {
// CHECK-NEXT: entry:
// CHECK-NEXT: bitcast %swift.bridge** %0 to %swift.refcounted**
// CHECK-NEXT: load %swift.refcounted*, %swift.refcounted** %1
// CHECK-NEXT: call i1 @_swift_isUniquelyReferencedOrPinned_nonNull_native(%swift.refcounted* %2)
// CHECK-NEXT: ret i1 %3
func isUniqueOrPinned_native(inout ref: Builtin.BridgeObject) -> Bool {
  return Builtin.isUniqueOrPinned_native(&ref)
}

// CHECK: ![[R]] = !{i64 0, i64 9223372036854775807}

