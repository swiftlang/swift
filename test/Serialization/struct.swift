// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/def_struct.swiftmodule %S/Inputs/def_struct.swift
// RUN: llvm-bcanalyzer %t/def_struct.swiftmodule | FileCheck %s
// RUN: %swift -emit-llvm -I=%t %s | FileCheck %s -check-prefix=LLVM

import def_struct

// LLVM: define {{.*}} @top_level_code() {
// LLVM: call void @_TV10def_struct5EmptyCfMS0_FT_S0_
// LLVM: }
var a : Empty

// CHECK:   # Toplevel Blocks: 5

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
// CHECK-NEXT:        Num Records: 7
// CHECK-NEXT:    Percent Abbrevs: 100.0000%

// CHECK:    		  Count    # Bits   %% Abv  Record Kind
// CHECK-NEXT:		      2  {{[0-9]+}} 100.00  NAME_HACK
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  DECL_CONTEXT
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  VAR_DECL
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  CONSTRUCTOR_DECL
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  STRUCT_DECL
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  STRUCT_TYPE

// CHECK:  Block ID #11 (INDEX_BLOCK):
// CHECK-NEXT:      Num Instances: 1
// CHECK-NEXT:         Total Size:
// CHECK-NEXT:    Percent of file:
// CHECK-NEXT:      Num SubBlocks: 0
// CHECK-NEXT:        Num Abbrevs: 2
// CHECK-NEXT:        Num Records: 3
// CHECK-NEXT:    Percent Abbrevs: 100.0000%

// CHECK:     		  Count    # Bits   %% Abv  Record Kind
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  TOP_LEVEL_DECLS
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  DECL_OFFSETS
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  TYPE_OFFSETS

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT

