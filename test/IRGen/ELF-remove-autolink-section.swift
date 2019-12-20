// RUN: %swiftc_driver -emit-ir %s -o - | %FileCheck %s -check-prefix ELF

// Check that the swift auto link section is available in the object file.
// RUN: %swiftc_driver -c %s -o %t
// RUN: llvm-readelf %t -S | %FileCheck %s -check-prefix SECTION

// Checks that the swift auto link section is removed from the final binary.
// RUN: %swiftc_driver  -emit-executable %s -o %t
// RUN: llvm-readelf %t -S | %FileCheck %s -check-prefix NOSECTION

// REQUIRES: OS=linux-gnu

print("Hi from Swift!")

// ELF: module asm ".section .swift1_autolink_entries,\220x80000000\22"
// SECTION: .swift1_autolink_entries
// NOSECTION-NOT: .swift1_autolink_entries
