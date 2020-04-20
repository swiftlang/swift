// RUN: rm -rf %t
// RUN: %empty-directory(%t/full)

// RUN: %target-swiftc_driver %S/Inputs/lto/lib.swift -lto=llvm-full -emit-library -emit-module -module-name A -working-directory %t/full
// RUN: %target-swiftc_driver %S/Inputs/lto/main.swift -L. -I. -lA -lto=llvm-full -working-directory %t/full
