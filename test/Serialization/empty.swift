// RUN: %swift -emit-module -o %t.sm %s
// RUN: llvm-bcanalyzer %t.sm | FileCheck %s


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
