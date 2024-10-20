// RUN: %target-swift-frontend -O -emit-ir -DU8 %s | %FileCheck %s -check-prefix=CHECK-U8
// RUN: %target-swift-frontend -O -emit-ir -DU16 %s | %FileCheck %s -check-prefix=CHECK-U16
// RUN: %target-swift-frontend -O -emit-ir -DU32 %s | %FileCheck %s -check-prefix=CHECK-U32
// RUN: %target-swift-frontend -O -emit-ir -DU64 %s | %FileCheck %s -check-prefix=CHECK-U64
// RUN: %target-swift-frontend -O -emit-ir -DUSIZE %s | %FileCheck %s -check-prefix=CHECK-USIZE-%target-ptrsize
// RUN: %target-swift-frontend -O -emit-ir -DI8 %s | %FileCheck %s -check-prefix=CHECK-I8
// RUN: %target-swift-frontend -O -emit-ir -DI16 %s | %FileCheck %s -check-prefix=CHECK-I16
// RUN: %target-swift-frontend -O -emit-ir -DI32 %s | %FileCheck %s -check-prefix=CHECK-I32
// RUN: %target-swift-frontend -O -emit-ir -DI64 %s | %FileCheck %s -check-prefix=CHECK-I64
// RUN: %target-swift-frontend -O -emit-ir -DISIZE %s | %FileCheck %s -check-prefix=CHECK-ISIZE-%target-ptrsize

#if U8
typealias IntType = UInt8
#elseif U16
typealias IntType = UInt16
#elseif U32
typealias IntType = UInt32
#elseif U64
typealias IntType = UInt64
#elseif USIZE
typealias IntType = UInt
#elseif I8
typealias IntType = Int8
#elseif I16
typealias IntType = Int16
#elseif I32
typealias IntType = Int32
#elseif I64
typealias IntType = Int64
#elseif ISIZE
typealias IntType = Int
#endif

// CHECK-U8: define {{.*}} swiftcc i8 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i8 {{.*}} [[N:%.*]])
// CHECK-U8:   [[ZERO_CMP:%.*]] = icmp eq i8 [[N]], 0
// CHECK-U8:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-U8: [[ADD_BB]]:
// CHECK-U8:   [[CUR:%.*]] = load i8, ptr [[DEST]]
// CHECK-U8:   [[ADD:%.*]] = add i8 [[CUR]], [[N]]
// CHECK-U8:   store i8 [[ADD]], ptr [[DEST]]
// CHECK-U8:   br label %[[DONE_BB]]
// CHECK-U8: [[DONE_BB]]:
// CHECK-U8:   ret i8 [[N]]

// CHECK-U16: define {{.*}} swiftcc i16 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i16 {{.*}} [[N:%.*]])
// CHECK-U16:   [[ZERO_CMP:%.*]] = icmp eq i16 [[N]], 0
// CHECK-U16:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-U16: [[ADD_BB]]:
// CHECK-U16:   [[CUR:%.*]] = load i16, ptr [[DEST]]
// CHECK-U16:   [[ADD:%.*]] = add i16 [[CUR]], [[N]]
// CHECK-U16:   store i16 [[ADD]], ptr [[DEST]]
// CHECK-U16:   br label %[[DONE_BB]]
// CHECK-U16: [[DONE_BB]]:
// CHECK-U16:   ret i16 [[N]]

// CHECK-U32: define {{.*}} swiftcc i32 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i32 {{.*}} [[N:%.*]])
// CHECK-U32:   [[ZERO_CMP:%.*]] = icmp eq i32 [[N]], 0
// CHECK-U32:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-U32: [[ADD_BB]]:
// CHECK-U32:   [[CUR:%.*]] = load i32, ptr [[DEST]]
// CHECK-U32:   [[ADD:%.*]] = add i32 [[CUR]], [[N]]
// CHECK-U32:   store i32 [[ADD]], ptr [[DEST]]
// CHECK-U32:   br label %[[DONE_BB]]
// CHECK-U32: [[DONE_BB]]:
// CHECK-U32:   ret i32 [[N]]

// CHECK-U64: define {{.*}} swiftcc i64 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i64 {{.*}} [[N:%.*]])
// CHECK-U64:   [[ZERO_CMP:%.*]] = icmp eq i64 [[N]], 0
// CHECK-U64:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-U64: [[ADD_BB]]:
// CHECK-U64:   [[CUR:%.*]] = load i64, ptr [[DEST]]
// CHECK-U64:   [[ADD:%.*]] = add i64 [[CUR]], [[N]]
// CHECK-U64:   store i64 [[ADD]], ptr [[DEST]]
// CHECK-U64:   br label %[[DONE_BB]]
// CHECK-U64: [[DONE_BB]]:
// CHECK-U64:   ret i64 [[N]]

// CHECK-USIZE-32: define {{.*}} swiftcc i32 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i32 {{.*}} [[N:%.*]])
// CHECK-USIZE-32:   [[ZERO_CMP:%.*]] = icmp eq i32 [[N]], 0
// CHECK-USIZE-32:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-USIZE-32: [[ADD_BB]]:
// CHECK-USIZE-32:   [[CUR:%.*]] = load i32, ptr [[DEST]]
// CHECK-USIZE-32:   [[ADD:%.*]] = add i32 [[CUR]], [[N]]
// CHECK-USIZE-32:   store i32 [[ADD]], ptr [[DEST]]
// CHECK-USIZE-32:   br label %[[DONE_BB]]
// CHECK-USIZE-32: [[DONE_BB]]:
// CHECK-USIZE-32:   ret i32 [[N]]

// CHECK-USIZE-64: define {{.*}} swiftcc i64 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i64 {{.*}} [[N:%.*]])
// CHECK-USIZE-64:   [[ZERO_CMP:%.*]] = icmp eq i64 [[N]], 0
// CHECK-USIZE-64:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-USIZE-64: [[ADD_BB]]:
// CHECK-USIZE-64:   [[CUR:%.*]] = load i64, ptr [[DEST]]
// CHECK-USIZE-64:   [[ADD:%.*]] = add i64 [[CUR]], [[N]]
// CHECK-USIZE-64:   store i64 [[ADD]], ptr [[DEST]]
// CHECK-USIZE-64:   br label %[[DONE_BB]]
// CHECK-USIZE-64: [[DONE_BB]]:
// CHECK-USIZE-64:   ret i64 [[N]]

// CHECK-I8: define {{.*}} swiftcc i8 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i8 {{.*}} [[N:%.*]])
// CHECK-I8:   [[NEGATIVE_CMP:%.*]] = icmp slt i8 [[N]], 0
// CHECK-I8:   br i1 [[NEGATIVE_CMP]], label %[[NEGATIVE_BB:.*]], label %[[NON_NEGATIVE_BB:.*]],
// CHECK-I8: [[NON_NEGATIVE_BB]]:
// CHECK-I8:   [[ZERO_CMP:%.*]] = icmp eq i8 [[N]], 0
// CHECK-I8:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-I8: [[ADD_BB]]:
// CHECK-I8:   [[CUR:%.*]] = load i8, ptr [[DEST]]
// CHECK-I8:   [[ADD:%.*]] = add i8 [[CUR]], [[N]]
// CHECK-I8:   store i8 [[ADD]], ptr [[DEST]]
// CHECK-I8:   br label %[[DONE_BB]]
// CHECK-I8: [[DONE_BB]]:
// CHECK-I8:   ret i8 [[N]]
// CHECK-I8: [[NEGATIVE_BB]]:
// CHECK-I8:   unreachable

// CHECK-I16: define {{.*}} swiftcc i16 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i16 {{.*}} [[N:%.*]])
// CHECK-I16:   [[NEGATIVE_CMP:%.*]] = icmp slt i16 [[N]], 0
// CHECK-I16:   br i1 [[NEGATIVE_CMP]], label %[[NEGATIVE_BB:.*]], label %[[NON_NEGATIVE_BB:.*]],
// CHECK-I16: [[NON_NEGATIVE_BB]]:
// CHECK-I16:   [[ZERO_CMP:%.*]] = icmp eq i16 [[N]], 0
// CHECK-I16:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-I16: [[ADD_BB]]:
// CHECK-I16:   [[CUR:%.*]] = load i16, ptr [[DEST]]
// CHECK-I16:   [[ADD:%.*]] = add i16 [[CUR]], [[N]]
// CHECK-I16:   store i16 [[ADD]], ptr [[DEST]]
// CHECK-I16:   br label %[[DONE_BB]]
// CHECK-I16: [[DONE_BB]]:
// CHECK-I16:   ret i16 [[N]]
// CHECK-I16: [[NEGATIVE_BB]]:
// CHECK-I16:   unreachable

// CHECK-I32: define {{.*}} swiftcc i32 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i32 {{.*}} [[N:%.*]])
// CHECK-I32:   [[NEGATIVE_CMP:%.*]] = icmp slt i32 [[N]], 0
// CHECK-I32:   br i1 [[NEGATIVE_CMP]], label %[[NEGATIVE_BB:.*]], label %[[NON_NEGATIVE_BB:.*]],
// CHECK-I32: [[NON_NEGATIVE_BB]]:
// CHECK-I32:   [[ZERO_CMP:%.*]] = icmp eq i32 [[N]], 0
// CHECK-I32:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-I32: [[ADD_BB]]:
// CHECK-I32:   [[CUR:%.*]] = load i32, ptr [[DEST]]
// CHECK-I32:   [[ADD:%.*]] = add i32 [[CUR]], [[N]]
// CHECK-I32:   store i32 [[ADD]], ptr [[DEST]]
// CHECK-I32:   br label %[[DONE_BB]]
// CHECK-I32: [[DONE_BB]]:
// CHECK-I32:   ret i32 [[N]]
// CHECK-I32: [[NEGATIVE_BB]]:
// CHECK-I32:   unreachable

// CHECK-I64: define {{.*}} swiftcc i64 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i64 {{.*}} [[N:%.*]])
// CHECK-I64:   [[NEGATIVE_CMP:%.*]] = icmp slt i64 [[N]], 0
// CHECK-I64:   br i1 [[NEGATIVE_CMP]], label %[[NEGATIVE_BB:.*]], label %[[NON_NEGATIVE_BB:.*]],
// CHECK-I64: [[NON_NEGATIVE_BB]]:
// CHECK-I64:   [[ZERO_CMP:%.*]] = icmp eq i64 [[N]], 0
// CHECK-I64:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-I64: [[ADD_BB]]:
// CHECK-I64:   [[CUR:%.*]] = load i64, ptr [[DEST]]
// CHECK-I64:   [[ADD:%.*]] = add i64 [[CUR]], [[N]]
// CHECK-I64:   store i64 [[ADD]], ptr [[DEST]]
// CHECK-I64:   br label %[[DONE_BB]]
// CHECK-I64: [[DONE_BB]]:
// CHECK-I64:   ret i64 [[N]]
// CHECK-I64: [[NEGATIVE_BB]]:
// CHECK-I64:   unreachable

// CHECK-ISIZE-32: define {{.*}} swiftcc i32 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i32 {{.*}} [[N:%.*]])
// CHECK-ISIZE-32:   [[NEGATIVE_CMP:%.*]] = icmp slt i32 [[N]], 0
// CHECK-ISIZE-32:   br i1 [[NEGATIVE_CMP]], label %[[NEGATIVE_BB:.*]], label %[[NON_NEGATIVE_BB:.*]],
// CHECK-ISIZE-32: [[NON_NEGATIVE_BB]]:
// CHECK-ISIZE-32:   [[ZERO_CMP:%.*]] = icmp eq i32 [[N]], 0
// CHECK-ISIZE-32:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-ISIZE-32: [[ADD_BB]]:
// CHECK-ISIZE-32:   [[CUR:%.*]] = load i32, ptr [[DEST]]
// CHECK-ISIZE-32:   [[ADD:%.*]] = add i32 [[CUR]], [[N]]
// CHECK-ISIZE-32:   store i32 [[ADD]], ptr [[DEST]]
// CHECK-ISIZE-32:   br label %[[DONE_BB]]
// CHECK-ISIZE-32: [[DONE_BB]]:
// CHECK-ISIZE-32:   ret i32 [[N]]
// CHECK-ISIZE-32: [[NEGATIVE_BB]]:
// CHECK-ISIZE-32:   unreachable

// CHECK-ISIZE-64: define {{.*}} swiftcc i64 @add_but_loop(ptr {{.*}} [[DEST:%.*]], i64 {{.*}} [[N:%.*]])
// CHECK-ISIZE-64:   [[NEGATIVE_CMP:%.*]] = icmp slt i64 [[N]], 0
// CHECK-ISIZE-64:   br i1 [[NEGATIVE_CMP]], label %[[NEGATIVE_BB:.*]], label %[[NON_NEGATIVE_BB:.*]],
// CHECK-ISIZE-64: [[NON_NEGATIVE_BB]]:
// CHECK-ISIZE-64:   [[ZERO_CMP:%.*]] = icmp eq i64 [[N]], 0
// CHECK-ISIZE-64:   br i1 [[ZERO_CMP]], label %[[DONE_BB:.*]], label %[[ADD_BB:.*]]
// CHECK-ISIZE-64: [[ADD_BB]]:
// CHECK-ISIZE-64:   [[CUR:%.*]] = load i64, ptr [[DEST]]
// CHECK-ISIZE-64:   [[ADD:%.*]] = add i64 [[CUR]], [[N]]
// CHECK-ISIZE-64:   store i64 [[ADD]], ptr [[DEST]]
// CHECK-ISIZE-64:   br label %[[DONE_BB]]
// CHECK-ISIZE-64: [[DONE_BB]]:
// CHECK-ISIZE-64:   ret i64 [[N]]
// CHECK-ISIZE-64: [[NEGATIVE_BB]]:
// CHECK-ISIZE-64:   unreachable

@_silgen_name("add_but_loop")
func add_but_loop(dest: inout IntType, n: IntType) -> IntType {
  var loops: IntType = 0

  for _ in 0 ..< n {
    dest &+= 1
    loops &+= 1
  }

  return loops
}
