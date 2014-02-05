// RUN: %swift_driver -driver-print-jobs -arch x86_64 %s 2>&1 > %t.simple.txt
// RUN: FileCheck %s < %t.simple.txt

// RUN: %swift_driver -driver-print-jobs -arch x86_64 %s -sdk %S/../Inputs/clang-importer-sdk -Xfrontend -foo -Xfrontend -bar 2>&1 > %t.complex.txt
// RUN: FileCheck %s < %t.complex.txt
// RUN: FileCheck -check-prefix COMPLEX %s < %t.complex.txt

// RUN: %swift_driver -driver-print-jobs -emit-silgen -arch x86_64 %s 2>&1 > %t.silgen.txt
// RUN: FileCheck %s < %t.silgen.txt
// RUN: FileCheck -check-prefix SILGEN %s < %t.silgen.txt

// RUN: %swift_driver -driver-print-jobs -emit-sil -arch x86_64 %s 2>&1 > %t.sil.txt
// RUN: FileCheck %s < %t.sil.txt
// RUN: FileCheck -check-prefix SIL %s < %t.sil.txt

// RUN: %swift_driver -driver-print-jobs -emit-ir -arch x86_64 %s 2>&1 > %t.ir.txt
// RUN: FileCheck %s < %t.ir.txt
// RUN: FileCheck -check-prefix IR %s < %t.ir.txt

// RUN: %swift_driver -driver-print-jobs -emit-bc -arch x86_64 %s 2>&1 > %t.bc.txt
// RUN: FileCheck %s < %t.bc.txt
// RUN: FileCheck -check-prefix BC %s < %t.bc.txt

// RUN: %swift_driver -driver-print-jobs -S -arch x86_64 %s 2>&1 > %t.s.txt
// RUN: FileCheck %s < %t.s.txt
// RUN: FileCheck -check-prefix ASM %s < %t.s.txt

// RUN: %swift_driver -driver-print-jobs -c -arch x86_64 %s 2>&1 > %t.c.txt
// RUN: FileCheck %s < %t.c.txt
// RUN: FileCheck -check-prefix OBJ %s < %t.c.txt

// REQUIRES: X86


// CHECK: bin/swift
// CHECK: driver-compile.swift
// CHECK: -o

// COMPLEX: bin/swift
// COMPLEX: -c
// COMPLEX: driver-compile.swift
// COMPLEX-DAG: -sdk {{.*}}/Inputs/clang-importer-sdk
// COMPLEX-DAG: -foo -bar
// COMPLEX: -o {{.+}}.o


// SILGEN: bin/swift
// SILGEN: -emit-silgen
// SILGEN: -o

// SIL: bin/swift
// SIL: -emit-sil{{ }}
// SIL: -o

// IR: bin/swift
// IR: -emit-ir
// IR: -o

// BC: bin/swift
// BC: -emit-bc
// BC: -o

// ASM: bin/swift
// ASM: -S{{ }}
// ASM: -o

// OBJ: bin/swift
// OBJ: -c{{ }}
// OBJ: -o
