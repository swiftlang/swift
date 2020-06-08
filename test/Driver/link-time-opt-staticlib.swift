// UNSUPPORTED: OS=windows-msvc
// RUN: rm -rf %t
// RUN: %empty-directory(%t/full-static)

// RUN: %target-swiftc_driver %S/Inputs/lto/lib.swift -static -lto=llvm-full -emit-library -emit-module -module-name A -working-directory %t/full-static
// RUN: %target-swiftc_driver %S/Inputs/lto/main.swift -L. -I. -lA -lto=llvm-full -working-directory %t/full-static
