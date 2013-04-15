// RUN: %swift %s -sil-irgen -emit-llvm -o - | FileCheck %s

import Builtin

// CHECK: [[REFCOUNT:%swift.refcounted.*]] = type
// CHECK: [[X:%C18sil_irgen_builtins1X]] = type
// CHECK: [[Y:%C18sil_irgen_builtins1Y]] = type

typealias Int = Builtin.Int32
typealias Bool = Builtin.Int1

operator infix * {
  associativity left
  precedence 200
}
operator infix / {
  associativity left
  precedence 200
}
operator infix % {
  associativity left
  precedence 200
}

operator infix + {
  associativity left
  precedence 190
}
operator infix - {
  associativity left
  precedence 190
}

operator infix << {
  associativity none
  precedence 180
}
operator infix >> {
  associativity none
  precedence 180
}

operator infix .. {
  associativity none
  precedence 175
}

operator infix < {
  associativity none
  precedence 170
}
operator infix <= {
  associativity none
  precedence 170
}
operator infix > {
  associativity none
  precedence 170
}
operator infix >= {
  associativity none
  precedence 170
}

operator infix == {
  associativity none
  precedence 160
}
operator infix != {
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
func < (lhs : Int, rhs : Int) -> Bool {
  return Builtin.cmp_slt_Int32(lhs, rhs)
  // CHECK: icmp slt i32
}
func > (lhs : Int, rhs : Int) -> Bool {
  return Builtin.cmp_sgt_Int32(lhs, rhs)
  // CHECK: icmp sgt i32
}
func <=(lhs : Int, rhs : Int) -> Bool {
  return Builtin.cmp_sle_Int32(lhs, rhs)
  // CHECK: icmp sle i32
}
func >=(lhs : Int, rhs : Int) -> Bool {
  return Builtin.cmp_sge_Int32(lhs, rhs)
  // CHECK: icmp sge i32
}
func ==(lhs : Int, rhs : Int) -> Bool {
  return Builtin.cmp_eq_Int32(lhs, rhs)
  // CHECK: icmp eq i32
}
func !=(lhs : Int, rhs : Int) -> Bool {
  return Builtin.cmp_ne_Int32(lhs, rhs)
  // CHECK: icmp ne i32
}

func gep_test(ptr : Builtin.RawPointer, offset : Builtin.Int64)
   -> Builtin.RawPointer {
  return Builtin.gep_Int64(ptr, offset)
  // CHECK: getelementptr inbounds i8*
}

// CHECK: define i64 @_T18sil_irgen_builtins9load_testFT3ptrBp_Bi64_
func load_test(ptr : Builtin.RawPointer) -> Builtin.Int64 {
  // CHECK: [[CASTPTR:%.*]] = bitcast i8* [[PTR:%.*]] to i64*
  // CHECK-NEXT: load i64* [[CASTPTR]]
  // CHECK: ret
  return Builtin.load(ptr)
}

// CHECK: define void @_T18sil_irgen_builtins11assign_testFT5valueBi64_3ptrBp_T_
func assign_test(value : Builtin.Int64, ptr : Builtin.RawPointer) {
  // CHECK: [[CASTPTR:%.*]] = bitcast i8* [[PTR:%.*]] to i64*
  // CHECK-NEXT: store i64 [[VALUE:%.*]], i64* [[CASTPTR]]
  Builtin.assign(value, ptr)
  // CHECK: ret
}

// CHECK: define %swift.refcounted* @_T18sil_irgen_builtins16load_object_testFT3ptrBp_Bo
func load_object_test(ptr : Builtin.RawPointer) -> Builtin.ObjectPointer {
  // CHECK: [[T0:%.*]] = load [[REFCOUNT]]**
  // CHECK: call void @swift_retain_noresult([[REFCOUNT]]* [[T0]])
  // CHECK: ret [[REFCOUNT]]* [[T0]]
  return Builtin.load(ptr)
}

// CHECK: define void @_T18sil_irgen_builtins18assign_object_testFT5valueBo3ptrBp_T_
func assign_object_test(value : Builtin.ObjectPointer, ptr : Builtin.RawPointer) {
  // CHECK: [[DEST:%.*]] = bitcast i8* %9 to %swift.refcounted**
  // CHECK-NEXT: [[OLD:%.*]] = load [[REFCOUNT]]** [[DEST]]
  // CHECK-NEXT: store [[REFCOUNT]]* {{%.*}}, [[REFCOUNT]]** [[DEST]]
  // CHECK-NEXT: call void @swift_release([[REFCOUNT]]* [[OLD]])
  Builtin.assign(value, ptr)
}

// CHECK: define void @_T18sil_irgen_builtins16init_object_testFT5valueBo3ptrBp_T_
func init_object_test(value : Builtin.ObjectPointer, ptr : Builtin.RawPointer) {
  // CHECK: [[DEST:%.*]] = bitcast i8* %9 to %swift.refcounted**
  // CHECK-NEXT: store [[REFCOUNT]]* {{%.*}}, [[REFCOUNT]]** [[DEST]]
  Builtin.init(value, ptr)
}

func cast_test() {
  var ptr : Builtin.RawPointer
  var i8  : Builtin.Int8
  var i64 : Builtin.Int64
  var f   : Builtin.FPIEEE32
  var d   : Builtin.FPIEEE64
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
}

func intrinsic_test() {
  Builtin.int_trap() // CHECK: llvm.trap()
  
  var i32 : Builtin.Int32
  i32 = Builtin.int_bswap_Int32(i32) // CHECK: llvm.bswap.i32(

  var i16 : Builtin.Int16
  i16 = Builtin.int_bswap_Int16(i16) // CHECK: llvm.bswap.i16(
  
  var x = Builtin.int_sadd_with_overflow_Int16(i16, i16) // CHECK: call { i16, i1 } @llvm.sadd.with.overflow.i16(
}

// CHECK: define void @_T18sil_irgen_builtins19sizeof_alignof_testFT_T_()
func sizeof_alignof_test() {
  // CHECK: store i64 4, i64*
  var xs = Builtin.sizeof(Int) 
  // CHECK: store i64 4, i64*
  var xa = Builtin.alignof(Int) 
  // CHECK: store i64 1, i64*
  var ys = Builtin.sizeof(Bool) 
  // CHECK: store i64 1, i64*
  var ya = Builtin.alignof(Bool) 

}

// CHECK: define void @_T18sil_irgen_builtins27generic_sizeof_alignof_testU__FT_T_(
func generic_sizeof_alignof_test<T>() {
  // CHECK:      [[T0:%.*]] = getelementptr inbounds i8** [[T:%.*]], i32 12
  // CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]]
  // CHECK-NEXT: [[SIZE:%.*]] = ptrtoint i8* [[T1]] to i64
  // CHECK-NEXT: store i64 [[SIZE]], i64* [[S:%.*]]
  var s = Builtin.sizeof(T)
  // CHECK: [[T0:%.*]] = getelementptr inbounds i8** [[T:%.*]], i32 13
  // CHECK-NEXT: [[T1:%.*]] = load i8** [[T0]]
  // CHECK-NEXT: [[ALIGN:%.*]] = ptrtoint i8* [[T1]] to i64
  // CHECK-NEXT: store i64 [[ALIGN]], i64* [[A:%.*]]
  var a = Builtin.alignof(T)
}

class X {}
func toObjectPointer(x : X) -> Builtin.ObjectPointer {
  // CHECK: @_T18sil_irgen_builtins15toObjectPointerFT1xCS_1X_Bo
  // CHECK-NOT: call void
  // CHECK: call void @swift_retain_noresult
  // CHECK-NOT: call void
  // CHECK: call void @swift_release
  // CHECK-NOT: call void
  // CHECK: ret
  return Builtin.castToObjectPointer(x)
}
func fromObjectPointer(x : Builtin.ObjectPointer) -> X {
  // CHECK: @_T18sil_irgen_builtins17fromObjectPointerFT1xBo_CS_1X
  // CHECK-NOT: call void
  // CHECK: call void @swift_retain_noresult
  // CHECK-NOT: call void
  // CHECK: call void @swift_release
  // CHECK-NOT: call void
  // CHECK: ret
  return Builtin.castFromObjectPointer(x)
}
func toRawPointer(x : X) -> Builtin.RawPointer {
  // CHECK: @_T18sil_irgen_builtins12toRawPointerFT1xCS_1X_Bp
  // CHECK-NOT: call void
  // CHECK: call void @swift_retain_noresult
  // CHECK-NOT: call void
  // CHECK: call void bitcast (void (%swift.refcounted*)* @swift_release
  // CHECK-NOT: call void
  // CHECK: call void @swift_release
  // CHECK-NOT: call void
  // CHECK: ret
  return Builtin.bridgeToRawPointer(x)
}
func fromRawPointer(x : Builtin.RawPointer) -> X {
  // CHECK: @_T18sil_irgen_builtins14fromRawPointerFT1xBp_CS_1X
  // CHECK-NOT: call void
  // CHECK: call void @swift_retain_noresult
  // CHECK-NOT: call void
  // CHECK: call void @swift_release
  // CHECK-NOT: call void
  // CHECK: ret
  return Builtin.bridgeFromRawPointer(x)
}

class Y {}
func move(ptr : Builtin.RawPointer) {
  var temp : Y = Builtin.move(ptr)
  // CHECK:      define void @_T18sil_irgen_builtins4moveFT3ptrBp_T_
  // CHECK:        [[SRC:%.*]] = bitcast i8* {{%.*}} to [[Y]]**
  // CHECK-NEXT:   [[VAL:%.*]] = load [[Y]]** [[SRC]]
  // CHECK-NEXT:   store [[Y]]* [[VAL]], [[Y]]** {{%.*}}
}

func destroy(ptr : Builtin.RawPointer) {
  Builtin.destroy(Y, ptr)
  // CHECK:    define void @_T18sil_irgen_builtins7destroyFT3ptrBp_T_
  // CHECK:      [[T0:%.*]] = load i8** {{%.*}}
  // CHECK-NEXT: [[T1:%.*]] = bitcast i8* [[T0]] to [[Y]]**
  // CHECK-NEXT: [[T2:%.*]] = load [[Y]]** [[T1]]
  // CHECK-NEXT: call void bitcast {{.*}} @swift_release
}

func allocDealloc(size : Builtin.Int64, align : Builtin.Int64) {
  // CHECK:    define void @_T18sil_irgen_builtins12allocDeallocFT4sizeBi64_5alignBi64__T_
  // CHECK:      [[T0:%.*]] = load i64* [[SIZE:%.*]], align 8
  // CHECK-NEXT: [[T1:%.*]] = load i64* [[ALIGN:%.*]], align 8
  // CHECK-NEXT: [[T2:%.*]] = call noalias i8* @swift_slowAlloc(i64 [[T0]], i64 2) [[NOUNWIND:#[0-9]+]]
  // CHECK-NEXT: store i8* [[T2]], i8** [[PTR:%.*]], align 8
  var ptr = Builtin.allocRaw(size, align)
  // CHECK:      [[T0:%.*]] = load i8** [[PTR]], align 8
  // CHECK-NEXT: [[T1:%.*]] = load i64* [[SIZE]], align 8
  // CHECK-NEXT: call void @swift_slowRawDealloc(i8* [[T0]], i64 [[T1]]) [[NOUNWIND]]
  Builtin.deallocRaw(ptr, size)
}

func fence_test() {
  // CHECK: fence acquire
  Builtin.fence_acquire()

  // CHECK: fence singlethread acq_rel
  Builtin.fence_acqrel_singlethread()
}

func cmpxchg_test(ptr : Builtin.RawPointer, a : Builtin.Int32, b : Builtin.Int32) {
  // CHECK: cmpxchg i32* {{.*}}, i32 {{.*}}, i32 {{.*}} acquire
  var z = Builtin.cmpxchg_acquire_Int32(ptr, a, b)

  // CHECK: cmpxchg volatile i32* {{.*}}, i32 {{.*}}, i32 {{.*}} monotonic
  var y = Builtin.cmpxchg_monotonic_volatile_Int32(ptr, a, b)
  
  // CHECK: cmpxchg volatile i32* {{.*}}, i32 {{.*}}, i32 {{.*}} singlethread acquire
  var x = Builtin.cmpxchg_acquire_volatile_singlethread_Int32(ptr, a, b)

  // rdar://12939803 - ER: support atomic cmpxchg/xchg with pointers
  // CHECK: cmpxchg volatile i64* {{.*}}, i64 {{.*}}, i64 {{.*}} singlethread acquire
  var w = Builtin.cmpxchg_acquire_volatile_singlethread_RawPointer(ptr, ptr, ptr)
}

func atomicrmw_test(ptr : Builtin.RawPointer, a : Builtin.Int32,
                    ptr2 : Builtin.RawPointer) {
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

func addressof_test(a:[byref] Int, b:[byref] Bool) {
  // CHECK: bitcast i32* {{.*}} to i8*
  var ap : Builtin.RawPointer = Builtin.addressof(&a)
  // CHECK: bitcast i1* {{.*}} to i8*
  var bp : Builtin.RawPointer = Builtin.addressof(&b)
}

func fneg_test(half:Builtin.FPIEEE16,
               single:Builtin.FPIEEE32,
               double:Builtin.FPIEEE64)
  -> (Builtin.FPIEEE16, Builtin.FPIEEE32, Builtin.FPIEEE64)
{
  // CHECK: fsub half 0xH8000, {{%.*}}
  // CHECK: fsub float -0.000000e+00, {{%.*}}
  // CHECK: fsub double -0.000000e+00, {{%.*}}
  return (Builtin.fneg_FPIEEE16(half),
          Builtin.fneg_FPIEEE32(single),
          Builtin.fneg_FPIEEE64(double))
}

// CHECK: attributes [[NOUNWIND]] = { nounwind }
