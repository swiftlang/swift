// RUN: %swift -emit-module -o %t.swiftmodule %s
// RUN: llvm-bcanalyzer %t.swiftmodule | FileCheck %s

import Builtin
typealias MyInt64 = Builtin.Int64
typealias AnotherInt64 = MyInt64

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
// CHECK-NEXT:        Num Records: 6
// CHECK-NEXT:    Percent Abbrevs: 100.0000%

// CHECK:    		  Count    # Bits   %% Abv  Record Kind
// CHECK-NEXT:		      2  {{[0-9]+}} 100.00  NAME_HACK
// CHECK-NEXT:		      2  {{[0-9]+}} 100.00  TYPE_ALIAS_DECL
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  NAME_ALIAS_TYPE
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  BUILTIN_TYPE

// CHECK:  Block ID #11 (INDEX_BLOCK):
// CHECK-NEXT:      Num Instances: 1
// CHECK-NEXT:         Total Size:
// CHECK-NEXT:    Percent of file:
// CHECK-NEXT:      Num SubBlocks: 0
// CHECK-NEXT:        Num Abbrevs: 1
// CHECK-NEXT:        Num Records: 2
// CHECK-NEXT:    Percent Abbrevs: 100.0000%

// CHECK:     		  Count    # Bits   %% Abv  Record Kind
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  DECL_OFFSETS
// CHECK-NEXT:		      1  {{[0-9]+}} 100.00  TYPE_OFFSETS

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT

