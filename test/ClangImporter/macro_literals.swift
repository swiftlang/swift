// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s | %FileCheck %s

import macros

// CHECK-LABEL: // testBitwiseOperations() -> ()
func testBitwiseOperations() {
  // CHECK: %0 = integer_literal $Builtin.Int64, -1, loc {{.*}}
  // CHECK-NEXT: %1 = struct $UInt64 (%0 : $Builtin.Int64), loc {{.*}}
  _ = DISPATCH_TIME_FOREVER as CUnsignedLongLong

  // CHECK-NEXT: %2 = integer_literal $Builtin.Int32, 1, loc {{.*}}
  // CHECK-NEXT: %3 = struct $Int32 (%2 : $Builtin.Int32), loc {{.*}}
  _ = BIT_SHIFT_1 as CInt
  // CHECK-NEXT: %4 = integer_literal $Builtin.Int32, 4, loc {{.*}}
  // CHECK-NEXT: %5 = struct $Int32 (%4 : $Builtin.Int32), loc {{.*}}
  _ = BIT_SHIFT_2 as CInt
  // CHECK-NEXT: %6 = integer_literal $Builtin.Int64, 24, loc {{.*}}
  // CHECK-NEXT: %7 = struct $Int64 (%6 : $Builtin.Int64), loc {{.*}}
  _ = BIT_SHIFT_3 as CLongLong
  // CHECK-NEXT: %8 = integer_literal $Builtin.Int32, 2, loc {{.*}}
  // CHECK-NEXT: %9 = struct $UInt32 (%8 : $Builtin.Int32), loc {{.*}}
  _ = BIT_SHIFT_4 as CUnsignedInt

  // CHECK-NEXT: %10 = integer_literal $Builtin.Int32, 1, loc {{.*}}
  // CHECK-NEXT: %11 = struct $UInt32 (%10 : $Builtin.Int32), loc {{.*}}
  _ = RSHIFT_ONE as CUnsignedInt
  // CHECK-NEXT: %12 = integer_literal $Builtin.Int32, -2, loc {{.*}}
  // CHECK-NEXT: %13 = struct $Int32 (%12 : $Builtin.Int32), loc {{.*}}
  _ = RSHIFT_NEG as CInt

  // CHECK-NEXT: %14 = integer_literal $Builtin.Int64, -4294967296, loc {{.*}}
  // CHECK-NEXT: %15 = struct $UInt64 (%14 : $Builtin.Int64), loc {{.*}}
  _ = XOR_HIGH as CUnsignedLongLong

  // CHECK-NEXT: %16 = integer_literal $Builtin.Int32, 256, loc {{.*}}
  // CHECK-NEXT: %17 = struct $Int32 (%16 : $Builtin.Int32), loc {{.*}}
  _ = ATTR_BOLD as CInt
  // CHECK-NEXT: %18 = integer_literal $Builtin.Int32, 512, loc {{.*}}
  // CHECK-NEXT: %19 = struct $Int32 (%18 : $Builtin.Int32), loc {{.*}}
  _ = ATTR_ITALIC as CInt
  // CHECK-NEXT: %20 = integer_literal $Builtin.Int32, 1024, loc {{.*}}
  // CHECK-NEXT: %21 = struct $Int32 (%20 : $Builtin.Int32), loc {{.*}}
  _ = ATTR_UNDERLINE as CInt
}

// CHECK-LABEL: // testIntegerArithmetic() -> ()
func testIntegerArithmetic() {

  // CHECK: %0 = integer_literal $Builtin.Int32, 0, loc {{.*}}
  // CHECK-NEXT: %1 = struct $Int32 (%0 : $Builtin.Int32), loc {{.*}}
  _ = ADD_ZERO as CInt
  // CHECK-NEXT: %2 = integer_literal $Builtin.Int32, 1, loc {{.*}}
  // CHECK-NEXT: %3 = struct $Int32 (%2 : $Builtin.Int32), loc {{.*}}
  _ = ADD_ONE as CInt
  // CHECK-NEXT: %4 = integer_literal $Builtin.Int32, 2, loc {{.*}}
  // CHECK-NEXT: %5 = struct $Int32 (%4 : $Builtin.Int32), loc {{.*}}
  _ = ADD_TWO as CInt
  // CHECK-NEXT: %6 = integer_literal $Builtin.Int32, -2, loc {{.*}}
  // CHECK-NEXT: %7 = struct $Int32 (%6 : $Builtin.Int32), loc {{.*}}
  _ = ADD_MINUS_TWO as CInt
  // CHECK-NEXT: %8 = integer_literal $Builtin.Int64, 169, loc {{.*}}
  // CHECK-NEXT: %9 = struct $Int64 (%8 : $Builtin.Int64), loc {{.*}}
  _ = ADD_MIXED_WIDTH as CLongLong
  // CHECK-NEXT: %10 = integer_literal $Builtin.Int64, 142, loc {{.*}}
  // CHECK-NEXT: %11 = struct $Int64 (%10 : $Builtin.Int64), loc {{.*}}
  _ = ADD_MIXED_SIGN as CLongLong
  // CHECK-NEXT: %12 = integer_literal $Builtin.Int32, -3, loc {{.*}}
  // CHECK-NEXT: %13 = struct $UInt32 (%12 : $Builtin.Int32), loc {{.*}}
  _ = ADD_UNDERFLOW as CUnsignedInt
  // CHECK-NEXT: %14 = integer_literal $Builtin.Int32, 2, loc {{.*}}
  // CHECK-NEXT: %15 = struct $UInt32 (%14 : $Builtin.Int32), loc {{.*}}
  _ = ADD_OVERFLOW as CUnsignedInt

  // CHECK-NEXT: %16 = integer_literal $Builtin.Int32, 1, loc {{.*}}
  // CHECK-NEXT: %17 = struct $Int32 (%16 : $Builtin.Int32), loc {{.*}}
  _ = SUB_ONE as CInt
  // CHECK-NEXT: %18 = integer_literal $Builtin.Int32, 0, loc {{.*}}
  // CHECK-NEXT: %19 = struct $Int32 (%18 : $Builtin.Int32), loc {{.*}}
  _ = SUB_ZERO as CInt
  // CHECK-NEXT: %20 = integer_literal $Builtin.Int32, -1, loc {{.*}}
  // CHECK-NEXT: %21 = struct $Int32 (%20 : $Builtin.Int32), loc {{.*}}
  _ = SUB_MINUS_ONE as CInt
  // CHECK-NEXT: %22 = integer_literal $Builtin.Int64, 42, loc {{.*}}
  // CHECK-NEXT: %23 = struct $Int64 (%22 : $Builtin.Int64), loc {{.*}}
  _ = SUB_MIXED_WIDTH as CLongLong
  // CHECK-NEXT: %24 = integer_literal $Builtin.Int32, 51, loc {{.*}}
  // CHECK-NEXT: %25 = struct $UInt32 (%24 : $Builtin.Int32), loc {{.*}}
  _ = SUB_MIXED_SIGN as CUnsignedInt
  // CHECK-NEXT: %26 = integer_literal $Builtin.Int32, -1, loc {{.*}}
  // CHECK-NEXT: %27 = struct $UInt32 (%26 : $Builtin.Int32), loc {{.*}}
  _ = SUB_UNDERFLOW as CUnsignedInt
  // CHECK-NEXT: %28 = integer_literal $Builtin.Int32, 1, loc {{.*}}
  // CHECK-NEXT: %29 = struct $UInt32 (%28 : $Builtin.Int32), loc {{.*}}
  _ = SUB_OVERFLOW as CUnsignedInt

  // CHECK-NEXT: %30 = integer_literal $Builtin.Int32, 36, loc {{.*}}
  // CHECK-NEXT: %31 = struct $Int32 (%30 : $Builtin.Int32), loc {{.*}}
  _ = MULT_POS as CInt
  // CHECK-NEXT: %32 = integer_literal $Builtin.Int32, -12, loc {{.*}}
  // CHECK-NEXT: %33 = struct $Int32 (%32 : $Builtin.Int32), loc {{.*}}
  _ = MULT_NEG as CInt
  // CHECK-NEXT: %34 = integer_literal $Builtin.Int64, 8589934590, loc {{.*}}
  // CHECK-NEXT: %35 = struct $Int64 (%34 : $Builtin.Int64), loc {{.*}}
  _ = MULT_MIXED_TYPES as CLongLong

  // CHECK-NEXT: %36 = integer_literal $Builtin.Int32, 128, loc {{.*}}
  // CHECK-NEXT: %37 = struct $Int32 (%36 : $Builtin.Int32), loc {{.*}}
  _ = DIVIDE_INTEGRAL as CInt
  // CHECK-NEXT: %38 = integer_literal $Builtin.Int32, 1, loc {{.*}}
  // CHECK-NEXT: %39 = struct $Int32 (%38 : $Builtin.Int32), loc {{.*}}
  _ = DIVIDE_NONINTEGRAL as CInt
  // CHECK-NEXT: %40 = integer_literal $Builtin.Int64, 2147483648, loc {{.*}}
  // CHECK-NEXT: %41 = struct $Int64 (%40 : $Builtin.Int64), loc {{.*}}
  _ = DIVIDE_MIXED_TYPES as CLongLong
}

// CHECK-LABEL: // testIntegerComparisons() -> ()
func testIntegerComparisons() {

  // CHECK: %0 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %1 = struct $Bool (%0 : $Builtin.Int1), loc {{.*}}
  _ = EQUAL_FALSE
  // CHECK-NEXT: %2 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %3 = struct $Bool (%2 : $Builtin.Int1), loc {{.*}}
  _ = EQUAL_TRUE
  // CHECK-NEXT: %4 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %5 = struct $Bool (%4 : $Builtin.Int1), loc {{.*}}
  _ = EQUAL_TRUE_MIXED_TYPES

  // CHECK-NEXT: %6 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %7 = struct $Bool (%6 : $Builtin.Int1), loc {{.*}}
  _ = GT_FALSE
  // CHECK-NEXT: %8 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %9 = struct $Bool (%8 : $Builtin.Int1), loc {{.*}}
  _ = GT_TRUE
  // CHECK-NEXT: %10 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %11 = struct $Bool (%10 : $Builtin.Int1), loc {{.*}}
  _ = GTE_FALSE
  // CHECK-NEXT: %12 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %13 = struct $Bool (%12 : $Builtin.Int1), loc {{.*}}
  _ = GTE_TRUE

  // CHECK-NEXT: %14 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %15 = struct $Bool (%14 : $Builtin.Int1), loc {{.*}}
  _ = LT_FALSE
  // CHECK-NEXT: %16 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %17 = struct $Bool (%16 : $Builtin.Int1), loc {{.*}}
  _ = LT_TRUE
  // CHECK-NEXT: %18 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %19 = struct $Bool (%18 : $Builtin.Int1), loc {{.*}}
  _ = LTE_FALSE
  // CHECK-NEXT: %20 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %21 = struct $Bool (%20 : $Builtin.Int1), loc {{.*}}
  _ = LTE_TRUE
}

// CHECK-LABEL: // testLogicalComparisons() -> ()
func testLogicalComparisons() {

  // CHECK: %0 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %1 = struct $Bool (%0 : $Builtin.Int1), loc {{.*}}
  _ = L_AND_TRUE
  // CHECK-NEXT: %2 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %3 = struct $Bool (%2 : $Builtin.Int1), loc {{.*}}
  _ = L_AND_FALSE
  // CHECK-NEXT: %4 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %5 = struct $Bool (%4 : $Builtin.Int1), loc {{.*}}
  _ = L_AND_TRUE_B
  // CHECK-NEXT: %6 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %7 = struct $Bool (%6 : $Builtin.Int1), loc {{.*}}
  _ = L_AND_FALSE_B

  // CHECK-NEXT: %8 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %9 = struct $Bool (%8 : $Builtin.Int1), loc {{.*}}
  _ = L_OR_TRUE
  // CHECK-NEXT: %10 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %11 = struct $Bool (%10 : $Builtin.Int1), loc {{.*}}
  _ = L_OR_FALSE
  // CHECK-NEXT: %12 = integer_literal $Builtin.Int1, -1, loc {{.*}}
  // CHECK-NEXT: %13 = struct $Bool (%12 : $Builtin.Int1), loc {{.*}}
  _ = L_OR_TRUE_B
  // CHECK-NEXT: %14 = integer_literal $Builtin.Int1, 0, loc {{.*}}
  // CHECK-NEXT: %15 = struct $Bool (%14 : $Builtin.Int1), loc {{.*}}
  _ = L_OR_FALSE_B
}
