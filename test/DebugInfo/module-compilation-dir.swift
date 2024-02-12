// Emit the explicit module.
// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-pcm -g -module-name Macro -o %t/Macro.pcm -file-compilation-dir /compdir %S/Inputs/module.modulemap
// RUN: %llvm-dwarfdump %t/Macro.pcm | %FileCheck %s

// CHECK: DW_AT_comp_dir	("/compdir")
