// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/def_func.swiftmodule %S/Inputs/def_func.swift
// RUN: llvm-bcanalyzer %t/def_func.swiftmodule | FileCheck %s
// RUN: %swift -emit-llvm -I=%t %s | FileCheck %s -check-prefix=LLVM

import def_func

// LLVM: define {{.*}} @top_level_code() {
// LLVM:   [[VAL:%.*]] = call i64 @_T8def_func7getZeroFT_Bi64_()
// LLVM:   store i64 [[VAL]], i64* @_T8function3rawBi64_, align 8
// LLVM: }
var raw = getZero()

// Check that 'raw' is a Builtin.Int64
var cooked = Int64(raw)

// CHECK:   # Toplevel Blocks: 6

// CHECK:  Block ID #0 (BLOCKINFO_BLOCK):
// CHECK-NEXT:      Num Instances: 1

// CHECK:  Block ID #8 (CONTROL_BLOCK):
// CHECK-NEXT:      Num Instances: 1
// CHECK-NEXT:         Total Size:
// CHECK-NEXT:    Percent of file:
// CHECK-NEXT:      Num SubBlocks: 0
// CHECK-NEXT:        Num Abbrevs: 1
// CHECK-NEXT:        Num Records: 1
// CHECK-NEXT:    Percent Abbrevs: 100.0000%

// CHECK:  Block ID #9 (INPUT_BLOCK):
// CHECK-NEXT:      Num Instances: 1
// CHECK-NEXT:         Total Size:
// CHECK-NEXT:    Percent of file:
// CHECK-NEXT:      Num SubBlocks: 0
// CHECK-NEXT:        Num Abbrevs: 1
// CHECK-NEXT:        Num Records: 1
// CHECK-NEXT:    Percent Abbrevs: 100.0000%
// CHECK:    		  Count    # Bits   %% Abv  Record Kind
// CHECK-NEXT: 		      1  {{[0-9]+}} 100.00  SOURCE_FILE

// CHECK:  Block ID #10 (DECLS_AND_TYPES_BLOCK):
// CHECK-NEXT:      Num Instances: 1
// CHECK-NEXT:         Total Size:
// CHECK-NEXT:    Percent of file:
// CHECK-NEXT:      Num SubBlocks: 0
// CHECK-NEXT:        Num Abbrevs:
// CHECK-NEXT:        Num Records:
// CHECK-NEXT:    Percent Abbrevs: 100.0000%

// CHECK:    		  Count    # Bits   %% Abv  Record Kind
// CHECK-NEXT:		     1  {{[0-9]+}} 100.00  FUNC_DECL
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  FUNCTION_TYPE
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  IDENTIFIER_TYPE
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  TUPLE_TYPE
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  BUILTIN_TYPE

// CHECK:  Block ID #11 (IDENTIFIER_DATA_BLOCK):
// CHECK-NEXT:      Num Instances: 1
// CHECK-NEXT:         Total Size:
// CHECK-NEXT:    Percent of file:
// CHECK-NEXT:      Num SubBlocks: 0
// CHECK-NEXT:        Num Abbrevs: 1
// CHECK-NEXT:        Num Records: 1
// CHECK-NEXT:    Percent Abbrevs: 100.0000%
// CHECK:    		  Count    # Bits   %% Abv  Record Kind
// CHECK-NEXT: 		      1  {{[0-9]+}} 100.00  IDENTIFIER_DATA

// CHECK:  Block ID #12 (INDEX_BLOCK):
// CHECK-NEXT:      Num Instances: 1
// CHECK-NEXT:         Total Size:
// CHECK-NEXT:    Percent of file:
// CHECK-NEXT:      Num SubBlocks: 0
// CHECK-NEXT:        Num Abbrevs: 2
// CHECK-NEXT:        Num Records: 4
// CHECK-NEXT:    Percent Abbrevs: 100.0000%

// CHECK:     		  Count    # Bits   %% Abv  Record Kind
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  TOP_LEVEL_DECLS
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  IDENTIFIER_OFFSETS
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  DECL_OFFSETS
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  TYPE_OFFSETS

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT

