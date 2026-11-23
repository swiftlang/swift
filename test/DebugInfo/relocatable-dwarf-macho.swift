// Embedded Mach-O targets relocate offsets from one DWARF section into another
// when emitting DWARF 5 or later, matching Clang. 32-bit ARM and x86 are
// excluded because their scattered relocations identify targets by address,
// and relocatable DWARF places every DWARF section at address 0.

// DEFINE: %{compile} = %swift -parse-stdlib -disable-legacy-type-info -g -c %s

// RUN: %empty-directory(%t)
// RUN: %{compile} -target arm64-apple-none-macho -o %t/arm64.o
// RUN: llvm-objdump -r --section=__debug_info %t/arm64.o \
// RUN:   | %FileCheck %s --check-prefix=RELOC

// RUN: %{compile} -target armv7em-apple-none-macho -o %t/armv7em.o
// RUN: llvm-objdump -r --section=__debug_info %t/armv7em.o \
// RUN:   | %FileCheck %s --check-prefix=PLAIN
// RUN: %{compile} -target i386-apple-none-macho -o %t/i386.o
// RUN: llvm-objdump -r --section=__debug_info %t/i386.o \
// RUN:   | %FileCheck %s --check-prefix=PLAIN
// RUN: %{compile} -target arm64-apple-none-macho -dwarf-version=4 -o %t/dwarf4.o
// RUN: llvm-objdump -r --section=__debug_info %t/dwarf4.o \
// RUN:   | %FileCheck %s --check-prefix=PLAIN
// RUN: %{compile} -target arm64-apple-macosx15.0 -o %t/macos.o
// RUN: llvm-objdump -r --section=__debug_info %t/macos.o \
// RUN:   | %FileCheck %s --check-prefix=PLAIN

// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: CODEGENERATOR=X86

public func f() {}

// RELOC:     RELOCATION RECORDS FOR [__debug_info]:
// RELOC-DAG: ARM64_RELOC_UNSIGNED __debug_abbrev
// RELOC-DAG: ARM64_RELOC_UNSIGNED __debug_line
// RELOC-DAG: ARM64_RELOC_UNSIGNED __debug_str_offs
// RELOC-DAG: ARM64_RELOC_UNSIGNED __debug_addr

// PLAIN:     file format mach-o
// PLAIN-NOT: RELOC_{{[A-Z_]+}} __debug_
