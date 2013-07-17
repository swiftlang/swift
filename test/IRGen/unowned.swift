// RUN: %swift -emit-llvm -triple=x86_64-apple-darwin10 %s | FileCheck %s

// CHECK: [[OPAQUE:%swift.opaque]] = type opaque
// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[REF:%swift.refcounted]] = type
// CHECK: [[C:%C7unowned1C]] = type { [[REF]] }
// CHECK: [[A:%V7unowned1A]] = type { [[C]]* }

class C {}

struct A {
  var [unowned] x : C
}

//   destroyBuffer
// CHECK:    define linkonce_odr hidden void @_TwXXV7unowned1A([[BUFFER:\[24 x i8\]]]* [[ARG:%.*]], [[TYPE]]*
// CHECK:      [[T0:%.*]] = bitcast [[BUFFER]]* [[ARG]] to [[A]]*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[A]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load [[C]]** [[T1]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_weakRelease to void ([[C]]*)*)([[C]]* [[T2]])
// CHECK-NEXT: ret void

//   initializeBufferWithCopyOfBuffer
// CHECK:    define linkonce_odr hidden [[OPAQUE]]* @_TwCPV7unowned1A([[BUFFER]]* [[DESTBUF:%.*]], [[BUFFER]]* [[SRCBUF:%.*]], [[TYPE]]*
// CHECK:      [[DEST:%.*]] = bitcast [[BUFFER]]* [[DESTBUF]] to [[A]]*
// CHECK-NEXT: [[SRC:%.*]] = bitcast [[BUFFER]]* [[SRCBUF]] to [[A]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[A]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[A]]* [[SRC]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load [[C]]** [[T1]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_weakRetain to void ([[C]]*)*)([[C]]* [[T2]])
// CHECK-NEXT: store [[C]]* [[T2]], [[C]]** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[A]]* [[DEST]] to [[OPAQUE]]*
// CHECK-NEXT: ret [[OPAQUE]]* [[T0]]

//   destroy
// CHECK:    define linkonce_odr hidden void @_TwxxV7unowned1A([[OPAQUE]]* [[ARG:%.*]], [[TYPE]]*
// CHECK:      [[T0:%.*]] = bitcast [[OPAQUE]]* [[ARG]] to [[A]]*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[A]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load [[C]]** [[T1]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_weakRelease to void ([[C]]*)*)([[C]]* [[T2]])
// CHECK-NEXT: ret void

//   initializeBufferWithCopy
// CHECK:    define linkonce_odr hidden [[OPAQUE]]* @_TwCpV7unowned1A([[BUFFER]]* [[DESTBUF:%.*]], [[OPAQUE]]* [[SRC_OPQ:%.*]], [[TYPE]]*
// CHECK:      [[SRC:%.*]] = bitcast [[OPAQUE]]* [[SRC_OPQ]] to [[A]]*
// CHECK-NEXT: [[DEST:%.*]] = bitcast [[BUFFER]]* [[DESTBUF]] to [[A]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[A]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[A]]* [[SRC]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load [[C]]** [[T1]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_weakRetain to void ([[C]]*)*)([[C]]* [[T2]])
// CHECK-NEXT: store [[C]]* [[T2]], [[C]]** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[A]]* [[DEST]] to [[OPAQUE]]*
// CHECK-NEXT: ret [[OPAQUE]]* [[T0]]

//   initializeWithCopy
// CHECK:    define linkonce_odr hidden [[OPAQUE]]* @_TwcpV7unowned1A([[OPAQUE]]* [[DEST_OPQ:%.*]], [[OPAQUE]]* [[SRC_OPQ:%.*]], [[TYPE]]*
// CHECK:      [[DEST:%.*]] = bitcast [[OPAQUE]]* [[DEST_OPQ]] to [[A]]*
// CHECK-NEXT: [[SRC:%.*]] = bitcast [[OPAQUE]]* [[SRC_OPQ]] to [[A]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[A]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[A]]* [[SRC]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load [[C]]** [[T1]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_weakRetain to void ([[C]]*)*)([[C]]* [[T2]])
// CHECK-NEXT: store [[C]]* [[T2]], [[C]]** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[A]]* [[DEST]] to [[OPAQUE]]*
// CHECK-NEXT: ret [[OPAQUE]]* [[T0]]

//   assignWithCopy
// CHECK:    define linkonce_odr hidden [[OPAQUE]]* @_TwcaV7unowned1A([[OPAQUE]]* [[DEST_OPQ:%.*]], [[OPAQUE]]* [[SRC_OPQ:%.*]], [[TYPE]]*
// CHECK:      [[DEST:%.*]] = bitcast [[OPAQUE]]* [[DEST_OPQ]] to [[A]]*
// CHECK-NEXT: [[SRC:%.*]] = bitcast [[OPAQUE]]* [[SRC_OPQ]] to [[A]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[A]]* [[SRC]], i32 0, i32 0
// CHECK-NEXT: [[NEW:%.*]] = load [[C]]** [[T0]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_weakRetain to void ([[C]]*)*)([[C]]* [[NEW]])
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[A]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: [[OLD:%.*]] = load [[C]]** [[T0]], align 8
// CHECK-NEXT: store [[C]]* [[NEW]], [[C]]** [[T0]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_weakRelease to void ([[C]]*)*)([[C]]* [[OLD]])
// CHECK-NEXT: [[T0:%.*]] = bitcast [[A]]* [[DEST]] to [[OPAQUE]]*
// CHECK-NEXT: ret [[OPAQUE]]* [[T0]]

//   assignWithTake
// CHECK:    define linkonce_odr hidden [[OPAQUE]]* @_TwtaV7unowned1A([[OPAQUE]]* [[DEST_OPQ:%.*]], [[OPAQUE]]* [[SRC_OPQ:%.*]], [[TYPE]]*
// CHECK:      [[DEST:%.*]] = bitcast [[OPAQUE]]* [[DEST_OPQ]] to [[A]]*
// CHECK-NEXT: [[SRC:%.*]] = bitcast [[OPAQUE]]* [[SRC_OPQ]] to [[A]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[A]]* [[SRC]], i32 0, i32 0
// CHECK-NEXT: [[NEW:%.*]] = load [[C]]** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[A]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: [[OLD:%.*]] = load [[C]]** [[T0]], align 8
// CHECK-NEXT: store [[C]]* [[NEW]], [[C]]** [[T0]], align 8
// CHECK-NEXT: call void bitcast (void ([[REF]]*)* @swift_weakRelease to void ([[C]]*)*)([[C]]* [[OLD]])
// CHECK-NEXT: [[T0:%.*]] = bitcast [[A]]* [[DEST]] to [[OPAQUE]]*
// CHECK-NEXT: ret [[OPAQUE]]* [[T0]]
